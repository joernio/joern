package io.joern.c2cpg.dataflow

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.dataflowengineoss.queryengine.condition.{ConditionAwareReachability, ConditionResult}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language.*

class ConditionAwareReachabilityTests extends C2CpgSuite {

  private val declarations = """
    |#define DVACT_RESUME 2
    |#define DVACT_WAKEUP 3
    |#define M_WAITOK 1
    |#define M_NOWAIT 0
    |void *malloc(unsigned long size, int type, int flags);
    |""".stripMargin

  private def reach(cpg: Cpg, name: String, phase: Int): ConditionResult = {
    val root = cpg.method.nameExact(name).filterNot(_.isExternal).head
    val act  = root.parameter.nameExact("act").head
    new ConditionAwareReachability(cpg).reachable(root, Map(act -> BigInt(phase)), _.name == "malloc")
  }

  private def flags(result: ConditionResult): Set[Option[BigInt]] =
    result.matches.map(_.arguments(2).constant).toSet

  "Condition-aware interprocedural reachability" should {

    "prune a callee branch using the actual argument of each invocation" in {
      val cpg = code(declarations + """
        |void helper(int create) {
        |  if (create)
        |    malloc(16, 0, M_WAITOK);
        |}
        |void guarded_activate(int act) {
        |  if (act == DVACT_RESUME)
        |    helper(0);
        |  if (act == DVACT_WAKEUP)
        |    helper(1);
        |}
        |""".stripMargin)

      reach(cpg, "guarded_activate", 2).matches shouldBe empty
      val wake = reach(cpg, "guarded_activate", 3)
      flags(wake) shouldBe Set(Some(BigInt(1)))
      wake.matches.exists(_.uncertain) shouldBe false
      wake.matches.head.path.map(_.name) should contain inOrderOnly ("helper", "malloc")
    }

    "preserve phase arguments through several helper calls" in {
      val cpg = code(declarations + """
        |void dispatch(int phase) {
        |  if (phase == DVACT_RESUME)
        |    malloc(16, 0, M_WAITOK);
        |}
        |void forward(int phase) {
        |  dispatch(phase);
        |}
        |void forwarded_activate(int act) {
        |  forward(act);
        |}
        |""".stripMargin)

      flags(reach(cpg, "forwarded_activate", 2)) shouldBe Set(Some(BigInt(1)))
      reach(cpg, "forwarded_activate", 3).matches shouldBe empty
    }

    "select switch labels while preserving fallthrough and default" in {
      val cpg = code(declarations + """
        |void switched_activate(int act) {
        |  int flags = M_NOWAIT;
        |  switch (act) {
        |  case DVACT_RESUME:
        |    flags = M_WAITOK;
        |  case DVACT_WAKEUP:
        |    malloc(16, 0, flags);
        |    break;
        |  default:
        |    malloc(32, 0, M_NOWAIT);
        |    break;
        |  }
        |}
        |""".stripMargin)

      flags(reach(cpg, "switched_activate", 2)) shouldBe Set(Some(BigInt(1)))
      flags(reach(cpg, "switched_activate", 3)) shouldBe Set(Some(BigInt(0)))
      val default = reach(cpg, "switched_activate", 9)
      flags(default) shouldBe Set(Some(BigInt(0)))
      default.matches.map(_.call.code).toSet shouldBe Set("malloc(32, 0, M_NOWAIT)")
    }

    "respect early returns and an empty true branch" in {
      val cpg = code(declarations + """
        |void returned_activate(int act) {
        |  if (act != DVACT_RESUME)
        |    return;
        |  if (act == DVACT_WAKEUP) {
        |  } else {
        |    malloc(16, 0, M_WAITOK);
        |  }
        |}
        |""".stripMargin)

      flags(reach(cpg, "returned_activate", 2)) shouldBe Set(Some(BigInt(1)))
      reach(cpg, "returned_activate", 3).matches shouldBe empty
    }

    "track a copied phase after the original parameter is changed" in {
      val cpg = code(declarations + """
        |void copied_activate(int act) {
        |  int original = act;
        |  act = DVACT_WAKEUP;
        |  if (original == DVACT_RESUME && act == DVACT_WAKEUP)
        |    malloc(16, 0, M_WAITOK);
        |}
        |""".stripMargin)

      flags(reach(cpg, "copied_activate", 2)) shouldBe Set(Some(BigInt(1)))
      reach(cpg, "copied_activate", 3).matches shouldBe empty
    }

    "apply helper return values to allocation flags" in {
      val cpg = code(declarations + """
        |int choose_flags(int act) {
        |  if (act == DVACT_RESUME)
        |    return M_WAITOK;
        |  return M_NOWAIT;
        |}
        |void returned_flags_activate(int act) {
        |  malloc(16, 0, choose_flags(act));
        |}
        |""".stripMargin)

      flags(reach(cpg, "returned_flags_activate", 2)) shouldBe Set(Some(BigInt(1)))
      flags(reach(cpg, "returned_flags_activate", 3)) shouldBe Set(Some(BigInt(0)))
    }

    "evaluate a condition using a callee return value" in {
      val cpg = code(declarations + """
        |int permitted(int phase) {
        |  return phase == DVACT_WAKEUP;
        |}
        |void returned_condition_activate(int act) {
        |  if (permitted(act))
        |    malloc(16, 0, M_WAITOK);
        |}
        |""".stripMargin)

      reach(cpg, "returned_condition_activate", 2).matches shouldBe empty
      flags(reach(cpg, "returned_condition_activate", 3)) shouldBe Set(Some(BigInt(1)))
    }

    "keep the same callee separate for different actual arguments" in {
      val cpg = code(declarations + """
        |void shared(int create, int flags) {
        |  if (create)
        |    malloc(16, 0, flags);
        |}
        |void first(int phase) {
        |  shared(phase == DVACT_RESUME, M_NOWAIT);
        |}
        |void second(int phase) {
        |  shared(phase == DVACT_WAKEUP, M_WAITOK);
        |}
        |void callers_activate(int act) {
        |  first(act);
        |  second(act);
        |}
        |""".stripMargin)

      flags(reach(cpg, "callers_activate", 2)) shouldBe Set(Some(BigInt(0)))
      val wake = reach(cpg, "callers_activate", 3)
      flags(wake) shouldBe Set(Some(BigInt(1)))
      wake.matches.head.path.map(_.name) should contain inOrderOnly ("second", "shared", "malloc")
    }

    "keep nonflag arguments separate from the allocation flags" in {
      val cpg = code(declarations + """
        |void arguments_activate(int act) {
        |  if (act == DVACT_RESUME) {
        |    int flags = M_NOWAIT;
        |    malloc(M_WAITOK, M_WAITOK, flags);
        |    malloc(16, 0, flags);
        |  }
        |}
        |""".stripMargin)

      val result = reach(cpg, "arguments_activate", 2)
      result.matches.size shouldBe 2
      flags(result) shouldBe Set(Some(BigInt(0)))
      result.matches.map(_.arguments(2).containsBits(BigInt(1))).toSet shouldBe Set(Some(false))
    }

    "evaluate flag masks without treating a flag test as a flag assignment" in {
      val cpg = code(declarations + """
        |void masks_activate(int act) {
        |  int flags = M_NOWAIT;
        |  if (flags & M_WAITOK)
        |    malloc(8, 0, M_WAITOK);
        |  if (act == DVACT_RESUME) {
        |    flags = M_WAITOK | 2;
        |    flags &= ~2;
        |    malloc(16, 0, flags);
        |  }
        |}
        |""".stripMargin)

      val result = reach(cpg, "masks_activate", 2)
      result.matches.size shouldBe 1
      flags(result) shouldBe Set(Some(BigInt(1)))
      result.matches.head.arguments(2).containsBits(BigInt(1)) shouldBe Some(true)
      reach(cpg, "masks_activate", 3).matches shouldBe empty
    }

    "join alternative returned flag values without inventing certainty" in {
      val cpg = code(declarations + """
        |int external_choice(void);
        |int choose(void) {
        |  if (external_choice())
        |    return M_WAITOK;
        |  return M_NOWAIT;
        |}
        |void alternative_activate(int act) {
        |  if (act == DVACT_RESUME)
        |    malloc(16, 0, choose());
        |}
        |""".stripMargin)

      val result = reach(cpg, "alternative_activate", 2)
      result.matches should not be empty
      result.matches.exists(_.uncertain) shouldBe true
      result.unknownCalls.map(_.name) should contain("external_choice")
      result.matches.forall(_.arguments(2).containsBits(BigInt(1)).contains(false)) shouldBe false
    }

    "invalidate a scalar passed by address to an unresolved call" in {
      val cpg = code(declarations + """
        |void change(int *value);
        |void changed_activate(int act) {
        |  int flags = M_NOWAIT;
        |  if (act == DVACT_RESUME) {
        |    change(&flags);
        |    malloc(16, 0, flags);
        |  }
        |}
        |""".stripMargin)

      val result = reach(cpg, "changed_activate", 2)
      result.matches should not be empty
      result.matches.forall(_.arguments(2).containsBits(BigInt(1)).contains(false)) shouldBe false
      result.matches.exists(_.uncertain) shouldBe true
      result.unknownCalls.map(_.name) should contain("change")
    }

    "report an unresolved callback instead of silently dropping it" in {
      val cpg = code(declarations + """
        |void callback_activate(int act, void (*callback)(int)) {
        |  if (act == DVACT_RESUME)
        |    callback(M_WAITOK);
        |}
        |""".stripMargin)

      val result = reach(cpg, "callback_activate", 2)
      result.unknownCalls.map(_.code) should contain("callback(M_WAITOK)")
      reach(cpg, "callback_activate", 3).unknownCalls shouldBe empty
    }

    "reach loop bodies without traversing unreachable code after an infinite loop" in {
      val cpg = code(declarations + """
        |void loop_activate(int act) {
        |  if (act == DVACT_RESUME) {
        |    while (1) {
        |      malloc(16, 0, M_WAITOK);
        |    }
        |    malloc(32, 0, M_WAITOK);
        |  }
        |}
        |""".stripMargin)

      val result = reach(cpg, "loop_activate", 2)
      result.matches.map(_.call.code).toSet shouldBe Set("malloc(16, 0, M_WAITOK)")
      flags(result) shouldBe Set(Some(BigInt(1)))
      reach(cpg, "loop_activate", 3).matches shouldBe empty
    }

    "propagate loop-carried scalar updates" in {
      val cpg = code(declarations + """
        |void loop_flags_activate(int act) {
        |  int flags = M_NOWAIT;
        |  if (act == DVACT_RESUME) {
        |    do {
        |      flags |= M_WAITOK;
        |    } while (0);
        |    malloc(16, 0, flags);
        |  }
        |}
        |""".stripMargin)

      flags(reach(cpg, "loop_flags_activate", 2)) shouldBe Set(Some(BigInt(1)))
      reach(cpg, "loop_flags_activate", 3).matches shouldBe empty
    }

    "discover flags introduced by a recursive invocation" in {
      val cpg = code(declarations + """
        |void recursive(int flags) {
        |  if (flags == M_NOWAIT)
        |    recursive(M_WAITOK);
        |  else
        |    malloc(16, 0, flags);
        |}
        |void recursive_activate(int act) {
        |  if (act == DVACT_RESUME)
        |    recursive(M_NOWAIT);
        |}
        |""".stripMargin)

      val result = reach(cpg, "recursive_activate", 2)
      flags(result) shouldBe Set(Some(BigInt(1)))
      result.matches.exists(_.uncertain) shouldBe false
      result.matches.head.path.map(_.name) shouldBe Vector("recursive", "recursive", "malloc")
      reach(cpg, "recursive_activate", 3).matches shouldBe empty
    }

    "terminate mutual recursion without a call-depth cutoff" in {
      val cpg = code(declarations + """
        |void second(int flags);
        |void first(int flags) {
        |  if (flags & M_WAITOK)
        |    malloc(16, 0, flags);
        |  second(flags | M_WAITOK);
        |}
        |void second(int flags) {
        |  first(flags);
        |}
        |void cycle_activate(int act) {
        |  if (act == DVACT_RESUME)
        |    first(M_NOWAIT);
        |}
        |""".stripMargin)

      val result = reach(cpg, "cycle_activate", 2)
      flags(result) shouldBe Set(Some(BigInt(1)))
      reach(cpg, "cycle_activate", 3).matches shouldBe empty
    }

    "follow a helper chain longer than an arbitrary traversal depth" in {
      val helpers = (0 until 64).reverse
        .map { index =>
          val body = if (index == 63) "malloc(16, 0, flags);" else s"helper${index + 1}(flags);"
          s"void helper$index(int flags) { $body }"
        }
        .mkString("\n")
      val cpg = code(declarations + helpers + """
        |void deep_activate(int act) {
        |  if (act == DVACT_RESUME)
        |    helper0(M_WAITOK);
        |}
        |""".stripMargin)

      val result = reach(cpg, "deep_activate", 2)
      flags(result) shouldBe Set(Some(BigInt(1)))
      result.matches.head.path.count(_.name.startsWith("helper")) shouldBe 64
      reach(cpg, "deep_activate", 3).matches shouldBe empty
    }

    "resolve explicit enum values without bypassing a shadowing local" in {
      val cpg = code(declarations + """
        |enum command { COMMAND_ALLOCATE = 4, COMMAND_SKIP = 5 };
        |void enum_helper(int command) {
        |  if (command == COMMAND_ALLOCATE)
        |    malloc(16, 0, M_WAITOK);
        |}
        |void enum_activate(int act) {
        |  if (act == DVACT_RESUME)
        |    enum_helper(COMMAND_ALLOCATE);
        |  if (act == DVACT_WAKEUP) {
        |    int COMMAND_ALLOCATE = 0;
        |    enum_helper(COMMAND_ALLOCATE);
        |  }
        |}
        |""".stripMargin)

      val resume = reach(cpg, "enum_activate", 2)
      flags(resume) shouldBe Set(Some(BigInt(1)))
      resume.matches.exists(_.uncertain) shouldBe false
      reach(cpg, "enum_activate", 3).matches shouldBe empty
    }

    "short circuit macro calls and surface missing argument provenance" in {
      val cpg = code(declarations + """
        |#define ALLOCATE(flags) allocate(flags)
        |int allocate(int flags) {
        |  malloc(16, 0, flags);
        |  return 1;
        |}
        |void short_circuit_activate(int act) {
        |  if (act == DVACT_RESUME && ALLOCATE(M_WAITOK)) {
        |  }
        |  if (act == DVACT_WAKEUP || ALLOCATE(M_NOWAIT)) {
        |  }
        |}
        |""".stripMargin)

      val resume = reach(cpg, "short_circuit_activate", 2)
      flags(resume) shouldBe Set(Some(BigInt(0)), Some(BigInt(1)))
      val waiting = resume.matches.filter(_.arguments(2).containsBits(BigInt(1)).contains(true))
      waiting should not be empty
      waiting.flatMap(_.origins(2)) shouldBe empty
      waiting.forall(_.provenanceUnknown(2)) shouldBe true
      reach(cpg, "short_circuit_activate", 3).matches shouldBe empty
    }

    "distinguish a named allocation flag origin from a bare numeric flag" in {
      val cpg = code(declarations + """
        |void origins_activate(int act) {
        |  if (act == DVACT_RESUME) {
        |    malloc(16, 0, M_WAITOK);
        |    malloc(32, 0, 1);
        |  }
        |}
        |""".stripMargin)

      val resume = reach(cpg, "origins_activate", 2)
      flags(resume) shouldBe Set(Some(BigInt(1)))
      val named = resume.matches.find(_.arguments.head.constant.contains(BigInt(16))).get
      named.origins(2).map(_.name) should contain("M_WAITOK")
      named.provenanceUnknown(2) shouldBe false
      val numeric = resume.matches.find(_.arguments.head.constant.contains(BigInt(32))).get
      numeric.origins(2) shouldBe empty
      numeric.provenanceUnknown(2) shouldBe false
      reach(cpg, "origins_activate", 3).matches shouldBe empty
    }

    "retain missing provenance when a function macro returns allocation flags" in {
      val cpg = code(declarations + """
        |#define FLAG_VALUE(flags) ((flags) | 2)
        |void macro_flags_activate(int act) {
        |  if (act == DVACT_RESUME)
        |    malloc(16, 0, FLAG_VALUE(M_WAITOK));
        |}
        |""".stripMargin)

      val resume = reach(cpg, "macro_flags_activate", 2)
      flags(resume) shouldBe Set(Some(BigInt(3)))
      resume.matches.flatMap(_.origins(2)).map(_.name).toSet should contain("FLAG_VALUE")
      resume.matches.forall(_.provenanceUnknown(2)) shouldBe true
      reach(cpg, "macro_flags_activate", 3).matches shouldBe empty
    }

    "refine a known flag value without adopting provenance from its predicate" in {
      val cpg = code(declarations + """
        |int numeric_flags(void) {
        |  return 1;
        |}
        |void predicate_origin_activate(int act) {
        |  if (act == DVACT_RESUME) {
        |    int flags = numeric_flags();
        |    if (flags == M_WAITOK)
        |      malloc(16, 0, flags);
        |  }
        |}
        |""".stripMargin)

      val resume = reach(cpg, "predicate_origin_activate", 2)
      flags(resume) shouldBe Set(Some(BigInt(1)))
      resume.matches.flatMap(_.origins(2)) shouldBe empty
      resume.matches.exists(_.uncertain) shouldBe false
      reach(cpg, "predicate_origin_activate", 3).matches shouldBe empty
    }

    "refine an unknown flag value without inventing a named flag source" in {
      val cpg = code(declarations + """
        |int runtime_flags(void);
        |void unknown_origin_activate(int act) {
        |  if (act == DVACT_RESUME) {
        |    int flags = runtime_flags();
        |    if (flags == M_WAITOK)
        |      malloc(16, 0, flags);
        |  }
        |}
        |""".stripMargin)

      val resume = reach(cpg, "unknown_origin_activate", 2)
      flags(resume) shouldBe Set(Some(BigInt(1)))
      resume.matches.flatMap(_.origins(2)) shouldBe empty
      resume.matches.forall(_.uncertain) shouldBe true
      resume.unknownCalls.map(_.name) should contain("runtime_flags")
      reach(cpg, "unknown_origin_activate", 3).matches shouldBe empty
    }

    "convert helper return values to the declared integer return type" in {
      val cpg = code(declarations + """
        |int truncated(void) {
        |  return 0x100000000ULL;
        |}
        |void truncated_return_activate(int act) {
        |  if (act == DVACT_RESUME && !truncated())
        |    malloc(16, 0, M_WAITOK);
        |}
        |""".stripMargin)

      val resume = reach(cpg, "truncated_return_activate", 2)
      flags(resume) shouldBe Set(Some(BigInt(1)))
      resume.matches.exists(_.uncertain) shouldBe false
      reach(cpg, "truncated_return_activate", 3).matches shouldBe empty
    }

    "convert a nonzero helper return to the declared boolean type" in {
      val cpg = code(declarations + """
        |_Bool boolean_value(void) {
        |  return 2;
        |}
        |void boolean_return_activate(int act) {
        |  if (act == DVACT_RESUME && boolean_value() == 1)
        |    malloc(16, 0, M_WAITOK);
        |}
        |""".stripMargin)

      val resume = reach(cpg, "boolean_return_activate", 2)
      flags(resume) shouldBe Set(Some(BigInt(1)))
      resume.matches.exists(_.uncertain) shouldBe false
      reach(cpg, "boolean_return_activate", 3).matches shouldBe empty
    }

    "retain an uncertain path after a helper mutates captured global storage" in {
      val cpg = code(declarations + """
        |int shared_state;
        |void change_shared(void) {
        |  shared_state = 1;
        |}
        |void shared_activate(int act) {
        |  if (act == DVACT_RESUME) {
        |    shared_state = 0;
        |    change_shared();
        |    if (shared_state)
        |      malloc(16, 0, M_WAITOK);
        |  }
        |}
        |""".stripMargin)

      val resume = reach(cpg, "shared_activate", 2)
      flags(resume) shouldBe Set(Some(BigInt(1)))
      resume.matches.forall(_.uncertain) shouldBe true
      reach(cpg, "shared_activate", 3).matches shouldBe empty
    }

    "retain static storage uncertainty across repeated helper invocations" in {
      val cpg = code(declarations + """
        |int visit(void) {
        |  static int seen = 0;
        |  if (seen)
        |    malloc(16, 0, M_WAITOK);
        |  seen = 1;
        |  return 0;
        |}
        |void static_activate(int act) {
        |  if (act == DVACT_RESUME) {
        |    visit();
        |    visit();
        |  }
        |}
        |""".stripMargin)

      val resume = reach(cpg, "static_activate", 2)
      flags(resume) shouldBe Set(Some(BigInt(1)))
      resume.matches.forall(_.uncertain) shouldBe true
      reach(cpg, "static_activate", 3).matches shouldBe empty
    }

    "mark allocations reached through unresolved computed goto targets uncertain" in {
      val cpg = code(declarations + """
        |void computed_activate(int act) {
        |  void *target = &&skip;
        |  if (act != DVACT_RESUME)
        |    return;
        |  goto *target;
        |allocate:
        |  malloc(16, 0, M_WAITOK);
        |  return;
        |skip:
        |  return;
        |}
        |""".stripMargin)

      val resume = reach(cpg, "computed_activate", 2)
      flags(resume) shouldBe Set(Some(BigInt(1)))
      resume.matches.forall(_.uncertain) shouldBe true
      reach(cpg, "computed_activate", 3).matches shouldBe empty
    }

    "retain helper results and their origins while evaluating other call arguments" in {
      val cpg = code(declarations + """
        |int waiting_flags(void) {
        |  return M_WAITOK;
        |}
        |int between(void) {
        |  return 0;
        |}
        |void argument_lifetime_activate(int act) {
        |  if (act == DVACT_RESUME) {
        |    malloc(waiting_flags(), between(), M_NOWAIT);
        |    malloc(16, between(), waiting_flags());
        |    malloc(waiting_flags(), between(), waiting_flags());
        |  }
        |}
        |""".stripMargin)

      val resume = reach(cpg, "argument_lifetime_activate", 2)
      resume.matches.size shouldBe 3
      resume.matches.flatMap(_.arguments).forall(_.constant.nonEmpty) shouldBe true
      flags(resume) shouldBe Set(Some(BigInt(0)), Some(BigInt(1)))
      val nonwaiting = resume.matches.find(_.arguments(2).constant.contains(BigInt(0))).get
      nonwaiting.arguments.head.constant shouldBe Some(BigInt(1))
      nonwaiting.origins.head.map(_.name) should contain("M_WAITOK")
      nonwaiting.origins(2).map(_.name) should not contain "M_WAITOK"
      val waiting = resume.matches.filter(_.arguments(2).constant.contains(BigInt(1)))
      waiting.forall(_.origins(2).exists(_.name == "M_WAITOK")) shouldBe true
      resume.matches.exists(_.uncertain) shouldBe false
      reach(cpg, "argument_lifetime_activate", 3).matches shouldBe empty
    }

    "retain a helper flag origin across unknown conditional alternatives" in {
      val cpg = code(declarations + """
        |int runtime_choice(void);
        |int waiting_flags(void) {
        |  return M_WAITOK;
        |}
        |int conditional_flags(void) {
        |  return runtime_choice() ? waiting_flags() : M_NOWAIT;
        |}
        |void conditional_origin_activate(int act) {
        |  if (act == DVACT_RESUME)
        |    malloc(16, 0, conditional_flags());
        |}
        |""".stripMargin)

      val resume = reach(cpg, "conditional_origin_activate", 2)
      resume.matches.size shouldBe 1
      resume.matches.head.arguments(2).containsBits(BigInt(1)) shouldBe None
      resume.matches.head.origins(2).map(_.name) should contain("M_WAITOK")
      resume.matches.head.uncertain shouldBe true
      resume.unknownCalls.map(_.name) should contain("runtime_choice")
      reach(cpg, "conditional_origin_activate", 3).matches shouldBe empty
    }

    "filter tracked sources without changing scalar values or splitting equivalent callers" in {
      val cpg = code(declarations + """
        |#define EXTRA_FLAG 2
        |#define FIRST_PHASE DVACT_RESUME
        |#define SECOND_PHASE DVACT_RESUME
        |int combined_flags(int phase) {
        |  if (phase == DVACT_RESUME)
        |    return M_WAITOK | EXTRA_FLAG;
        |  return M_NOWAIT;
        |}
        |void source_filter_activate(int act) {
        |  if (act == DVACT_RESUME) {
        |    malloc(16, 0, combined_flags(FIRST_PHASE));
        |    malloc(32, 0, combined_flags(SECOND_PHASE));
        |  }
        |}
        |""".stripMargin)

      val root   = cpg.method.nameExact("source_filter_activate").head
      val act    = root.parameter.nameExact("act").head
      val engine = new ConditionAwareReachability(cpg, source = _.name == "M_WAITOK")
      val resume = engine.reachable(root, Map(act -> BigInt(2)), _.name == "malloc")
      resume.matches.size shouldBe 2
      flags(resume) shouldBe Set(Some(BigInt(3)))
      resume.matches.map(_.origins(2).map(_.name)).toSet shouldBe Set(Set("M_WAITOK"))
      resume.matches.exists(_.uncertain) shouldBe false
      resume.contexts.count(_.method.name == "combined_flags") shouldBe 1
      engine.reachable(root, Map(act -> BigInt(3)), _.name == "malloc").matches shouldBe empty
    }

    "execute a copied function macro argument call only through its expansion" in {
      val cpg = code(declarations + """
        |#define ONCE(value) ((value) + 1)
        |int allocate(void) {
        |  malloc(16, 0, M_WAITOK);
        |  return 1;
        |}
        |void macro_call_activate(int act) {
        |  if (act == DVACT_RESUME) {
        |    int result = ONCE(allocate());
        |  }
        |}
        |""".stripMargin)

      val resume = reach(cpg, "macro_call_activate", 2)
      resume.edges.count(_.call.name == "allocate") shouldBe 1
      resume.matches.size shouldBe 1
      flags(resume) shouldBe Set(Some(BigInt(1)))
      resume.matches.exists(_.uncertain) shouldBe false
      reach(cpg, "macro_call_activate", 3).matches shouldBe empty
    }

    "infer scalar types for an assignment side effect in a macro expansion" in {
      val cpg = code(declarations + """
        |#define ONCE(value) ((value) + 1)
        |void macro_effect_activate(int act) {
        |  if (act == DVACT_RESUME) {
        |    int flags = 0;
        |    int result = ONCE(flags=flags^1);
        |    if (flags == 1 && result == 2)
        |      malloc(16, 0, M_WAITOK);
        |  }
        |}
        |""".stripMargin)

      val resume = reach(cpg, "macro_effect_activate", 2)
      flags(resume) shouldBe Set(Some(BigInt(1)))
      resume.matches.exists(_.uncertain) shouldBe false
      reach(cpg, "macro_effect_activate", 3).matches shouldBe empty
    }

    "stop at a configured terminal call without entering its body or following statements" in {
      val cpg = code(declarations + """
        |void panic(void) {
        |  malloc(8, 0, M_WAITOK);
        |}
        |void terminal_activate(int act) {
        |  panic();
        |  malloc(16, 0, M_WAITOK);
        |}
        |""".stripMargin)

      val root   = cpg.method.nameExact("terminal_activate").head
      val act    = root.parameter.nameExact("act").head
      val engine = new ConditionAwareReachability(cpg)
      engine.reachable(root, Map(act -> BigInt(2)), _.name == "malloc", _.name == "panic").matches shouldBe empty
      reach(cpg, "terminal_activate", 2).matches.map(_.call.code).toSet shouldBe Set(
        "malloc(8, 0, M_WAITOK)",
        "malloc(16, 0, M_WAITOK)"
      )
    }

    "stop caller continuation when a helper reaches a configured terminal call" in {
      val cpg = code(declarations + """
        |void panic(void) {
        |  malloc(8, 0, M_WAITOK);
        |}
        |void fatal_helper(void) {
        |  panic();
        |  malloc(16, 0, M_WAITOK);
        |}
        |void terminal_helper_activate(int act) {
        |  fatal_helper();
        |  malloc(32, 0, M_WAITOK);
        |}
        |""".stripMargin)

      val root   = cpg.method.nameExact("terminal_helper_activate").head
      val act    = root.parameter.nameExact("act").head
      val engine = new ConditionAwareReachability(cpg)
      engine.reachable(root, Map(act -> BigInt(2)), _.name == "malloc", _.name == "panic").matches shouldBe empty
    }

    "preserve alternate branches when a configured terminal call stops one branch" in {
      val cpg = code(declarations + """
        |void panic(void) {
        |  malloc(8, 0, M_WAITOK);
        |}
        |int external_choice(void);
        |void terminal_branch_activate(int act) {
        |  if (act == DVACT_RESUME)
        |    panic();
        |  malloc(16, 0, M_WAITOK);
        |}
        |void terminal_unknown_activate(int act) {
        |  if (external_choice())
        |    panic();
        |  malloc(32, 0, M_WAITOK);
        |}
        |""".stripMargin)

      val root   = cpg.method.nameExact("terminal_branch_activate").head
      val act    = root.parameter.nameExact("act").head
      val engine = new ConditionAwareReachability(cpg)
      engine.reachable(root, Map(act -> BigInt(2)), _.name == "malloc", _.name == "panic").matches shouldBe empty
      val wake = engine.reachable(root, Map(act -> BigInt(3)), _.name == "malloc", _.name == "panic")
      wake.matches.map(_.call.code).toSet shouldBe Set("malloc(16, 0, M_WAITOK)")
      wake.matches.exists(_.uncertain) shouldBe false

      val unknownRoot = cpg.method.nameExact("terminal_unknown_activate").head
      val unknownAct  = unknownRoot.parameter.nameExact("act").head
      val unknown = engine.reachable(unknownRoot, Map(unknownAct -> BigInt(2)), _.name == "malloc", _.name == "panic")
      unknown.matches.map(_.call.code).toSet shouldBe Set("malloc(32, 0, M_WAITOK)")
      unknown.matches.forall(_.uncertain) shouldBe true
    }

    "evaluate allocation arguments before a configured terminal call" in {
      val cpg = code(declarations + """
        |void panic(void *value) {
        |  malloc(8, 0, M_WAITOK);
        |}
        |void terminal_argument_activate(int act) {
        |  panic(malloc(16, 0, M_WAITOK));
        |  malloc(32, 0, M_WAITOK);
        |}
        |""".stripMargin)

      val root   = cpg.method.nameExact("terminal_argument_activate").head
      val act    = root.parameter.nameExact("act").head
      val engine = new ConditionAwareReachability(cpg)
      val result = engine.reachable(root, Map(act -> BigInt(2)), _.name == "malloc", _.name == "panic")
      result.matches.map(_.call.code).toSet shouldBe Set("malloc(16, 0, M_WAITOK)")
      result.matches.head.path.map(_.name) shouldBe Vector("malloc")
    }

    "propagate uncertainty from a callee return to following caller statements" in {
      val cpg = code(declarations + """
        |int external_choice(void);
        |void maybe_returns(void) {
        |  if (external_choice())
        |    return;
        |  while (1) {
        |  }
        |}
        |void uncertain_return_activate(int act) {
        |  if (act == DVACT_RESUME) {
        |    maybe_returns();
        |    malloc(16, 0, M_WAITOK);
        |  }
        |}
        |""".stripMargin)

      val resume = reach(cpg, "uncertain_return_activate", 2)
      flags(resume) shouldBe Set(Some(BigInt(1)))
      resume.matches.forall(_.uncertain) shouldBe true
      resume.unknownCalls.map(_.name) should contain("external_choice")
      reach(cpg, "uncertain_return_activate", 3).matches shouldBe empty
    }
  }
}
