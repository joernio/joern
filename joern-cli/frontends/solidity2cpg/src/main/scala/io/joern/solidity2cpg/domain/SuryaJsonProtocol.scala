package io.joern.solidity2cpg.domain
import io.joern.solidity2cpg.domain.SuryaObject._
import org.json4s.native.Json
import org.slf4j.LoggerFactory
import spray.json.{DefaultJsonProtocol, JsArray, JsBoolean, JsFalse, JsNull, JsNumber, JsObject, JsString, JsValue, JsonFormat, jsonReader}

/** Manually decodes Surya generated JSON objects to their assigned case classes. For more information see:
  * @see
  *   https://github.com/spray/spray-json
  */
object SuryaJsonProtocol extends DefaultJsonProtocol {
  private val logger = LoggerFactory.getLogger(SuryaJsonProtocol.getClass)

  /** Given a base AST node, will decide which class it belongs to and decode it. If the type is not supported a
    * [[BaseASTNode]] containing the type will be returned.
    */
  implicit object BaseASTNodeJsonFormat extends JsonFormat[BaseASTNode] with DefaultJsonProtocol {
    def write(c: BaseASTNode): JsValue = JsNull

    /** Parses the JSON value and determines which [[BaseASTNode]] class this is. Whenever a class references nested
      * [[BaseASTNode]]s then it needs it's own JSON format e.g. [[SourceUnitJsonFormat]] or
      * [[ContractDefinitionJsonFormat]].
      * @param json
      *   the JSON value to parse.
      * @return
      *   a specific [[SuryaObject]] case class or a [[BaseASTNode]] if the class is unhandled.
      */
    def read(json: JsValue): BaseASTNode = {
      println()
      println("json: ",json.convertTo[JsValue])
//      if (json == "null") {
//          json = JsNull
//      }
      val fields = json.asJsObject("BaseASTNode object expected").fields
      val typ    = fields("type").convertTo[String]
      typ match {
        case "SourceUnit" =>
          SourceUnitJsonFormat.read(json)
        case "PragmaDirective" =>
          PragmaDirectiveJsonFormat.read(json)
        case "ImportDirective" =>
          ImportDirectiveJsonFormat.read(json)
        case "ContractDefinition" =>
          ContractDefinitionJsonFormat.read(json)
        case "InheritanceSpecifier" =>
          InheritanceSpecifierJsonFormat.read(json)
        case "UserDefinedTypeName" =>
          UserDefinedTypeNameJsonFormat.read(json)
        case "ModifierDefinition" =>
          ModifierDefinitionJsonFormat.read(json)
        case "VariableDeclaration" =>
          VariableDeclarationJsonFormat.read(json)
        case "ElementaryTypeName" =>
          ElementaryTypeNameJsonFormat.read(json)
        case "Identifier" =>
          IdentifierJsonFormat.read(json)
        case "Block" =>
          BlockJsonFormat.read(json)
        case "ExpressionStatement" =>
          ExpressionStatementJsonFormat.read(json)
        case "FunctionCall" =>
          FunctionCallJsonFormat.read(json)
        case "MemberAccess" =>
          MemberAccessJsonFormat.read(json)
        case "IndexAccess" =>
          IndexAccessJsonFormat.read(json)
        case "BinaryOperation" =>
          BinaryOperationJsonFormat.read(json)
        case "StringLiteral" =>
          StringLiteralJsonFormat.read(json)
        case "EventDefinition" =>
          EventDefinitionJsonFormat.read(json)
        case "FunctionDefinition" =>
          FunctionDefinitionJsonFormat.read(json)
        case "ModifierInvocation" =>
          ModifierInvocationJsonFormat.read(json)
        case "EmitStatement" =>
          EmitStatementJsonFormat.read(json)
        case "ForStatement" =>
          ForStatementJsonFormat.read(json)
        case "VariableDeclarationStatement" =>
          VariableDeclarationStatementJsonFormat.read(json)
        case "UnaryOperation" =>
          UnaryOperationJsonFormat.read(json)
        case "IfStatement" =>
          IfStatementJsonFormat.read(json)
        case "BooleanLiteral" =>
          BooleanLiteralJsonFormat.read(json)
        case "ArrayTypeName" =>
          ArrayTypeNameJsonFormat.read(json)
        case "NumberLiteral" =>
          NumberLiteralJsonFormat.read(json)
        case "StateVariableDeclaration" =>
          StateVariableDeclarationJsonFormat.read(json)
        case "Mapping" =>
          MappingJsonFormat.read(json)
        case "StructDefinition" =>
          StructDefinitionJsonFormat.read(json)
        case "UsingForDeclaration" =>
          UsingForDeclarationJsonFormat.read(json)
        case "ReturnStatement" =>
          ReturnStatementJsonFormat.read(json)
        case "TupleExpression" =>
          TupleExpressionJsonFormat.read(json)
        case "FunctionTypeName" =>
          FunctionTypeNameJsonFormat.read(json)
        case "NewExpression" =>
          NewExpressionJsonFormat.read(json)
        case "TypeNameExpression" =>
          TypeNameExpressionJsonFormat.read(json)
        case "InlineAssemblyStatement" =>
          InlineAssemblyStatementJsonFormat.read(json)
        case "AssemblyBlock" =>
          AssemblyBlockJsonFormat.read(json)
        case "AssemblyAssignment" =>
          AssemblyAssignmentJsonFormat.read(json)
        case "AssemblyCall" =>
          AssemblyCallJsonFormat.read(json)
        case "DecimalNumber" =>
          DecimalNumberJsonFormat.read(json)
        case "ThrowStatement" =>
          ThrowStatementJsonFormat.read(json)
        case "Conditional" =>
          ConditionalJsonFormat.read(json)
        case _ =>
          logger.warn(s"Unhandled type '$typ' parsed from JSON AST.");
          new BaseASTNode(`type` = fields("type").convertTo[String])

      }
    }
  }

  implicit object SourceUnitJsonFormat extends JsonFormat[SourceUnit] with DefaultJsonProtocol {

    def write(c: SourceUnit): JsValue = JsNull

    def read(json: JsValue): SourceUnit = {
      val fields = json.asJsObject("Unable to decode JSON as SourceUnit").fields
      if (fields("type").convertTo[String] != "SourceUnit") {
        throw new RuntimeException("SourceUnit object expected")
      } else {
        SourceUnit(fields("children").convertTo[List[BaseASTNode]])
      }
    }
  }

  implicit object PragmaDirectiveJsonFormat extends JsonFormat[PragmaDirective] with DefaultJsonProtocol {

    def write(c: PragmaDirective): JsValue = JsNull

    def read(json: JsValue): PragmaDirective = {
      val fields = json.asJsObject("Unable to decode JSON as PragmaDirective").fields
      if (fields("type").convertTo[String] != "PragmaDirective") {
        throw new RuntimeException("PragmaDirective object expected")
      } else {
        PragmaDirective(fields("name").convertTo[String], fields("value").convertTo[String])
      }
    }
  }

  implicit object ImportDirectiveJsonFormat extends JsonFormat[ImportDirective] with DefaultJsonProtocol {

    def write(c: ImportDirective): JsValue = JsNull

    def read(json: JsValue): ImportDirective = {
      val fields = json.asJsObject("Unable to decode JSON as ImportDirective").fields
      if (fields("type").convertTo[String] != "ImportDirective") {
        throw new RuntimeException("ImportDirective object expected")
      } else {
        ImportDirective(
          fields("path").convertTo[String],
          fields("unitAlias") match {
            case x: JsString => x.convertTo[String]
            case _           => null
          },
          fields("unitAliasIdentifier") match {
            case x: JsString => x.convertTo[String]
            case _           => null
          },
          fields("symbolAliases") match {
            case x: JsString => x.convertTo[String]
            case _           => null
          },
          fields("symbolAliasesIdentifiers") match {
            case x: JsString => x.convertTo[String]
            case _           => null
          }
        )
      }
    }
  }

  implicit object ContractDefinitionJsonFormat extends JsonFormat[ContractDefinition] with DefaultJsonProtocol {

    def write(c: ContractDefinition): JsValue = JsNull

    def read(json: JsValue): ContractDefinition = {
      val fields = json.asJsObject("Unable to decode JSON as ContractDefinition").fields
      if (fields("type").convertTo[String] != "ContractDefinition") {
        throw new RuntimeException("ContractDefinition object expected")
      } else {
        ContractDefinition(
          fields("name").convertTo[String],
          fields("baseContracts").convertTo[List[BaseASTNode]],
          fields("subNodes").convertTo[List[BaseASTNode]],
          fields("kind").convertTo[String]
        )
      }
    }
  }

  implicit object BaseNameJsonFormat extends JsonFormat[BaseName] with DefaultJsonProtocol {

    def write(c: BaseName): JsValue = JsNull

    def read(json: JsValue): BaseName = {
      val fields = json.asJsObject("Unable to decode JSON as BaseName").fields
      BaseName(fields("type").convertTo[String], fields("namePath").convertTo[String])
    }

  }

  implicit object InheritanceSpecifierJsonFormat extends JsonFormat[InheritanceSpecifier] with DefaultJsonProtocol {

    def write(c: InheritanceSpecifier): JsValue = JsNull

    def read(json: JsValue): InheritanceSpecifier = {
      val fields = json.asJsObject("Unable to decode JSON as InheritanceSpecifier").fields
      if (fields("type").convertTo[String] != "InheritanceSpecifier") {
        throw new RuntimeException("InheritanceSpecifier object expected")
      } else {
        InheritanceSpecifier(fields("baseName").convertTo[BaseName], fields("arguments").convertTo[List[BaseASTNode]])
      }
    }
  }

  implicit object UserDefinedTypeNameJsonFormat extends JsonFormat[UserDefinedTypeName] with DefaultJsonProtocol {

    def write(c: UserDefinedTypeName): JsValue = JsNull

    def read(json: JsValue): UserDefinedTypeName = {
      val fields = json.asJsObject("Unable to decode JSON as UserDefinedTypeName").fields
      if (fields("type").convertTo[String] != "UserDefinedTypeName") {
        throw new RuntimeException("UserDefinedTypeName object expected")
      } else {
        UserDefinedTypeName(fields("namePath").convertTo[String])
      }
    }
  }

  implicit object ModifierDefinitionJsonFormat extends JsonFormat[ModifierDefinition] with DefaultJsonProtocol {

    def write(c: ModifierDefinition): JsValue = JsNull

    def read(json: JsValue): ModifierDefinition = {
      val fields = json.asJsObject("Unable to decode JSON as ModifierDefinition").fields
      if (fields("type").convertTo[String] != "ModifierDefinition") {
        throw new RuntimeException("ModifierDefinition object expected")
      } else {
        ModifierDefinition(
          fields("name").convertTo[String],
          fields("parameters") match {
            case x: JsArray => x.convertTo[List[BaseASTNode]]
            case _          => null
          },
          fields("body").convertTo[BaseASTNode],
          fields("isVirtual").convertTo[Boolean],
          fields("override") match {
            case x: JsObject => x.convertTo[BaseASTNode]
            case _           => null
          }
        )
      }
    }
  }

  implicit object VariableDeclarationJsonFormat extends JsonFormat[VariableDeclaration] with DefaultJsonProtocol {

    def write(c: VariableDeclaration): JsValue = JsNull

    def read(json: JsValue): VariableDeclaration = {
      val fields = json.asJsObject("Unable to decode JSON as VariableDeclaration").fields
      if (fields("type").convertTo[String] != "VariableDeclaration") {
        throw new RuntimeException("VariableDeclaration object expected")
      } else {
        try {
          VariableDeclaration(
            fields("typeName").convertTo[BaseASTNode],
            fields("name") match {
              case x: JsString => x.convertTo[String]
              case _ => null
            },
            fields("identifier") match {
              case x: JsObject => x.convertTo[BaseASTNode]
              case _ => null
            },
            fields("expression") match {
              case JsNull => null
              case x: JsObject => x.convertTo[BaseASTNode]
              case _ => null
            },
            fields("visibility") match {
              case x: JsString => x.convertTo[String]
              case _ => null
            },
            fields("storageLocation") match {
              case x: JsString => x.convertTo[String]
              case _ => null
            },
            fields("isStateVar").convertTo[Boolean],
            fields("isDeclaredConst").convertTo[Boolean],
            fields("isIndexed").convertTo[Boolean],
            fields("isImmutable").convertTo[Boolean],
            fields("override") match {
              case x: JsObject => x.convertTo[BaseASTNode]
              case _ => null
            }
//            ,
//            fields("lineNumber") match {
//              case JsNumber(value) => Option(value.toIntExact)
//              case JsNull => None
//            },
//            fields("columnNumber") match {
//              case JsNumber(value) => Option(value.toIntExact)
//              case JsNull => None
//            }
          )
        } catch {
          case e : NoSuchElementException => {
            VariableDeclaration(
              fields("typeName").convertTo[BaseASTNode],
              fields("name") match {
                case x: JsString => x.convertTo[String]
                case _ => null
              },
              fields("identifier") match {
                case x: JsObject => x.convertTo[BaseASTNode]
                case _ => null
              },
              fields("expression") match {
                case x: JsObject => x.convertTo[BaseASTNode]
                case _ => null
              },
              null,
              fields("storageLocation") match {
                case x: JsString => x.convertTo[String]
                case _ => null
              },
              fields("isStateVar").convertTo[Boolean],
              isDeclaredConst = false,
              isIndexed = fields("isIndexed").convertTo[Boolean],
              isImmutable = false,
              null
            )
          }
        }

      }
    }
  }

  implicit object ElementaryTypeNameJsonFormat extends JsonFormat[ElementaryTypeName] with DefaultJsonProtocol {

    def write(c: ElementaryTypeName): JsValue = JsNull

    def read(json: JsValue): ElementaryTypeName = {
      val fields = json.asJsObject("Unable to decode JSON as ElementaryTypeName").fields
      if (fields("type").convertTo[String] != "ElementaryTypeName") {
        throw new RuntimeException("ElementaryTypeName object expected")
      } else {
        ElementaryTypeName(
          fields("name") match {
            case x: JsString => x.convertTo[String]
            case _           => null
          },
          fields("stateMutability") match {
            case x: JsString => x.convertTo[String]
            case _           => null
          }
        )
      }
    }
  }

  implicit object IdentifierJsonFormat extends JsonFormat[Identifier] with DefaultJsonProtocol {

    def write(c: Identifier): JsValue = JsNull

    def read(json: JsValue): Identifier = {
      val fields = json.asJsObject("Unable to decode JSON as Identifier").fields
      if (fields("type").convertTo[String] != "Identifier") {
        throw new RuntimeException("Identifier object expected")
      } else {
        Identifier(fields("name").convertTo[String])
      }
    }
  }

  implicit object BlockJsonFormat extends JsonFormat[Block] with DefaultJsonProtocol {

    def write(c: Block): JsValue = JsNull

    def read(json: JsValue): Block = {
      val fields = json.asJsObject("Unable to decode JSON as Block").fields
      if (fields("type").convertTo[String] != "Block") {
        throw new RuntimeException("Block object expected")
      } else {
        Block(fields("statements").convertTo[List[BaseASTNode]])
      }
    }
  }

  implicit object ExpressionStatementJsonFormat extends JsonFormat[ExpressionStatement] with DefaultJsonProtocol {

    def write(c: ExpressionStatement): JsValue = JsNull

    def read(json: JsValue): ExpressionStatement = {
      val fields = json.asJsObject("Unable to decode JSON as ExpressionStatement").fields
      if (fields("type").convertTo[String] != "ExpressionStatement") {
        throw new RuntimeException("ExpressionStatement object expected")
      } else {
        ExpressionStatement(fields("expression").convertTo[BaseASTNode])
      }
    }
  }

  implicit object FunctionCallJsonFormat extends JsonFormat[FunctionCall] with DefaultJsonProtocol {

    def write(c: FunctionCall): JsValue = JsNull

    def read(json: JsValue): FunctionCall = {
      val fields = json.asJsObject("Unable to decode JSON as FunctionCall").fields
      if (fields("type").convertTo[String] != "FunctionCall") {
        throw new RuntimeException("FunctionCall object expected")
      } else {
        FunctionCall(
          fields("expression").convertTo[BaseASTNode],
          fields("arguments") match {
            case x: JsArray => x.convertTo[List[BaseASTNode]]
            case _           => null
          },
          fields("names").convertTo[List[String]],
          fields("identifiers").convertTo[List[String]],
          fields.getOrElse("methodFullName", null) match {
            case x: JsString => x.convertTo[String]
            case _ => null
          }
        )
      }
    }
  }

  implicit object MemberAccessJsonFormat extends JsonFormat[MemberAccess] with DefaultJsonProtocol {

    def write(c: MemberAccess): JsValue = JsNull

    def read(json: JsValue): MemberAccess = {
      val fields = json.asJsObject("Unable to decode JSON as MemberAccess").fields
      if (fields("type").convertTo[String] != "MemberAccess") {
        throw new RuntimeException("MemberAccess object expected")
      } else {
        MemberAccess(fields("expression").convertTo[BaseASTNode], fields("memberName").convertTo[String])
      }
    }
  }

  implicit object IndexAccessJsonFormat extends JsonFormat[IndexAccess] with DefaultJsonProtocol {

    def write(c: IndexAccess): JsValue = JsNull

    def read(json: JsValue): IndexAccess = {
      val fields = json.asJsObject("Unable to decode JSON as IndexAccess").fields
      if (fields("type").convertTo[String] != "IndexAccess") {
        throw new RuntimeException("IndexAccess object expected")
      } else {
        IndexAccess(fields("base").convertTo[BaseASTNode], fields("index").convertTo[BaseASTNode])
      }
    }
  }

  implicit object BinaryOperationJsonFormat extends JsonFormat[BinaryOperation] with DefaultJsonProtocol {

    def write(c: BinaryOperation): JsValue = JsNull

    def read(json: JsValue): BinaryOperation = {
      val fields = json.asJsObject("Unable to decode JSON as BinaryOperation").fields
      if (fields("type").convertTo[String] != "BinaryOperation") {
        throw new RuntimeException("BinaryOperation object expected")
      } else {
        BinaryOperation(
          fields("operator").convertTo[String],
          fields("left").convertTo[BaseASTNode],
          fields("right").convertTo[BaseASTNode]
        )
      }
    }
  }

  implicit object StringLiteralJsonFormat extends JsonFormat[StringLiteral] with DefaultJsonProtocol {

    def write(c: StringLiteral): JsValue = JsNull

    def read(json: JsValue): StringLiteral = {
      val fields = json.asJsObject("Unable to decode JSON as StringLiteral").fields
      if (fields("type").convertTo[String] != "StringLiteral") {
        throw new RuntimeException("StringLiteral object expected")
      } else {
        StringLiteral(
          fields("value").convertTo[String],
          fields("parts").convertTo[List[String]],
          fields("isUnicode") match {
            case x: List[JsBoolean] => x
            case _                  => null
          }
        )
      }
    }
  }

  implicit object EventDefinitionJsonFormat extends JsonFormat[EventDefinition] with DefaultJsonProtocol {

    def write(c: EventDefinition): JsValue = JsNull

    def read(json: JsValue): EventDefinition = {
      val fields = json.asJsObject("Unable to decode JSON as EventDefinition").fields
      if (fields("type").convertTo[String] != "EventDefinition") {
        throw new RuntimeException("EventDefinition object expected")
      } else {
        EventDefinition(
          fields("name").convertTo[String],
          fields("parameters").convertTo[List[BaseASTNode]],
          fields("isAnonymous").convertTo[Boolean]
        )
      }
    }
  }

  implicit object FunctionDefinitionJsonFormat extends JsonFormat[FunctionDefinition] with DefaultJsonProtocol {

    def write(c: FunctionDefinition): JsValue = JsNull

    def read(json: JsValue): FunctionDefinition = {
      val fields = json.asJsObject("Unable to decode JSON as FunctionDefinition").fields
      if (fields("type").convertTo[String] != "FunctionDefinition") {
        throw new RuntimeException("FunctionDefinition object expected")
      } else {
        FunctionDefinition(
          fields("name") match {
            case x: JsString => x.convertTo[String]
            case _ => null
          },
          fields("parameters").convertTo[List[BaseASTNode]],
          fields("returnParameters") match {
            case x: JsArray => x.convertTo[List[BaseASTNode]]
            case _ => null
          },
          fields("body").convertTo[BaseASTNode],
          fields("visibility").convertTo[String],
          fields("modifiers").convertTo[List[BaseASTNode]],
          fields("override") match {
            case x: JsObject => x.convertTo[BaseASTNode]
            case _           => null
          },
          fields("isConstructor").convertTo[Boolean],
          fields("isReceiveEther").convertTo[Boolean],
          fields("isFallback").convertTo[Boolean],
          fields("isVirtual").convertTo[Boolean],
          fields("stateMutability") match {
            case x: JsString => x.convertTo[String]
            case _           => null
          }
        )
      }
    }
  }

  implicit object ModifierInvocationJsonFormat extends JsonFormat[ModifierInvocation] with DefaultJsonProtocol {

    def write(c: ModifierInvocation): JsValue = JsNull

    def read(json: JsValue): ModifierInvocation = {
      val fields = json.asJsObject("Unable to decode JSON as ModifierInvocation").fields
      if (fields("type").convertTo[String] != "ModifierInvocation") {
        throw new RuntimeException("ModifierInvocation object expected")
      } else {
        ModifierInvocation(
          fields("name").convertTo[String],
          fields("arguments") match {
            case x: JsArray => x.convertTo[List[BaseASTNode]]
            case _           => null
          }
        )
      }
    }
  }

  implicit object EmitStatementJsonFormat extends JsonFormat[EmitStatement] with DefaultJsonProtocol {

    def write(c: EmitStatement): JsValue = JsNull

    def read(json: JsValue): EmitStatement = {
      val fields = json.asJsObject("Unable to decode JSON as EmitStatement").fields
      if (fields("type").convertTo[String] != "EmitStatement") {
        throw new RuntimeException("EmitStatement object expected")
      } else {
        EmitStatement(fields("eventCall").convertTo[BaseASTNode])
      }
    }
  }

  implicit object ForStatementJsonFormat extends JsonFormat[ForStatement] with DefaultJsonProtocol {

    def write(c: ForStatement): JsValue = JsNull

    def read(json: JsValue): ForStatement = {
      val fields = json.asJsObject("Unable to decode JSON as ForStatement").fields
      if (fields("type").convertTo[String] != "ForStatement") {
        throw new RuntimeException("ForStatement object expected")
      } else {
        ForStatement(
          fields("initExpression").convertTo[BaseASTNode],
          fields("conditionExpression").convertTo[BaseASTNode],
          fields("loopExpression").convertTo[BaseASTNode],
          fields("body").convertTo[BaseASTNode]
        )
      }
    }
  }

  implicit object VariableDeclarationStatementJsonFormat
      extends JsonFormat[VariableDeclarationStatement]
      with DefaultJsonProtocol {

    def write(c: VariableDeclarationStatement): JsValue = JsNull

    def read(json: JsValue): VariableDeclarationStatement = {
      val fields = json.asJsObject("Unable to decode JSON as VariableDeclarationStatement").fields
      if (fields("type").convertTo[String] != "VariableDeclarationStatement") {
        throw new RuntimeException("VariableDeclarationStatement object expected")
      } else {
//        print(fields("variables"))
        VariableDeclarationStatement(
          fields("variables").convertTo[List[BaseASTNode]],
          fields("initialValue") match {
            case JsNull => null
            case x: JsObject => x.convertTo[BaseASTNode]

          }
        )
      }
    }
  }

  implicit object UnaryOperationJsonFormat extends JsonFormat[UnaryOperation] with DefaultJsonProtocol {

    def write(c: UnaryOperation): JsValue = JsNull

    def read(json: JsValue): UnaryOperation = {
      val fields = json.asJsObject("Unable to decode JSON as UnaryOperation").fields
      if (fields("type").convertTo[String] != "UnaryOperation") {
        throw new RuntimeException("UnaryOperation object expected")
      } else {
        UnaryOperation(
          fields("operator").convertTo[String],
          fields("subExpression").convertTo[BaseASTNode],
          fields("isPrefix").convertTo[Boolean]
        )
      }
    }
  }

  implicit object IfStatementJsonFormat extends JsonFormat[IfStatement] with DefaultJsonProtocol {

    def write(c: IfStatement): JsValue = JsNull

    def read(json: JsValue): IfStatement = {
      val fields = json.asJsObject("Unable to decode JSON as IfStatement").fields
      if (fields("type").convertTo[String] != "IfStatement") {
        throw new RuntimeException("IfStatement object expected")
      } else {
        IfStatement(
          fields("condition").convertTo[BaseASTNode],
          fields("trueBody").convertTo[BaseASTNode],
          fields("falseBody") match {
            case x: JsObject => x.convertTo[BaseASTNode]
            case _           => null
          }
        )
      }
    }
  }

  implicit object BooleanLiteralJsonFormat extends JsonFormat[BooleanLiteral] with DefaultJsonProtocol {

    def write(c: BooleanLiteral): JsValue = JsNull

    def read(json: JsValue): BooleanLiteral = {
      val fields = json.asJsObject("Unable to decode JSON as BooleanLiteral").fields
      if (fields("type").convertTo[String] != "BooleanLiteral") {
        throw new RuntimeException("BooleanLiteral object expected")
      } else {
        BooleanLiteral(fields("value").convertTo[Boolean])
      }
    }
  }

  implicit object ArrayTypeNameJsonFormat extends JsonFormat[ArrayTypeName] with DefaultJsonProtocol {

    def write(c: ArrayTypeName): JsValue = JsNull

    def read(json: JsValue): ArrayTypeName = {
      val fields = json.asJsObject("Unable to decode JSON as ArrayTypeName").fields
      if (fields("type").convertTo[String] != "ArrayTypeName") {
        throw new RuntimeException("ArrayTypeName object expected")
      } else {
        ArrayTypeName(
          fields("baseTypeName").convertTo[BaseASTNode],
          fields("length") match {
            case x: JsString => x.convertTo[String]
            case _           => null
          }
        )
      }
    }
  }

  implicit object NumberLiteralJsonFormat extends JsonFormat[NumberLiteral] with DefaultJsonProtocol {

    def write(c: NumberLiteral): JsValue = JsNull

    def read(json: JsValue): NumberLiteral = {
      val fields = json.asJsObject("Unable to decode JSON as NumberLiteral").fields
      if (fields("type").convertTo[String] != "NumberLiteral") {
        throw new RuntimeException("NumberLiteral object expected")
      } else {
        NumberLiteral(
          fields("number").convertTo[String],
          fields("subdenomination") match {
            case x: JsString => x.convertTo[String]
            case _           => null
          }
        )
      }
    }
  }

  implicit object StateVariableDeclarationJsonFormat
      extends JsonFormat[StateVariableDeclaration]
      with DefaultJsonProtocol {

    def write(c: StateVariableDeclaration): JsValue = JsNull

    def read(json: JsValue): StateVariableDeclaration = {
      val fields = json.asJsObject("Unable to decode JSON as StateVariableDeclaration").fields
      if (fields("type").convertTo[String] != "StateVariableDeclaration") {
        throw new RuntimeException("StateVariableDeclaration object expected")
      } else {
        StateVariableDeclaration(
          fields("variables").convertTo[List[BaseASTNode]],
          fields("initialValue") match {
            case x: JsObject => x.convertTo[BaseASTNode]
            case _ => null

          }
        )
      }
    }

  }

  implicit object MappingJsonFormat extends JsonFormat[Mapping] with DefaultJsonProtocol {

    def write(c: Mapping): JsValue = JsNull

    def read(json: JsValue): Mapping = {
      val fields = json.asJsObject("Unable to decode JSON as Mapping").fields
      if (fields("type").convertTo[String] != "Mapping") {
        throw new RuntimeException("Mapping object expected")
      } else {
        Mapping(fields("keyType").convertTo[BaseASTNode], fields("valueType").convertTo[BaseASTNode])
      }
    }
  }

  implicit object StructDefinitionJsonFormat extends JsonFormat[StructDefinition] with DefaultJsonProtocol {

    def write(c: StructDefinition): JsValue = JsNull

    def read(json: JsValue): StructDefinition = {
      val fields = json.asJsObject("Unable to decode JSON as StructDefinition").fields
      if (fields("type").convertTo[String] != "StructDefinition") {
        throw new RuntimeException("StructDefinition object expected")
      } else {
        StructDefinition(fields("name").convertTo[String], fields("members").convertTo[List[BaseASTNode]])
      }
    }
  }

  implicit object UsingForDeclarationJsonFormat extends JsonFormat[UsingForDeclaration] with DefaultJsonProtocol {

    def write(c: UsingForDeclaration): JsValue = JsNull

    def read(json: JsValue): UsingForDeclaration = {
      val fields = json.asJsObject("Unable to decode JSON as UsingForDeclaration").fields
      if (fields("type").convertTo[String] != "UsingForDeclaration") {
        throw new RuntimeException("UsingForDeclaration object expected")
      } else {
        UsingForDeclaration(fields("typeName").convertTo[BaseASTNode], fields("libraryName").convertTo[String])
      }
    }
  }

  implicit object ReturnStatementJsonFormat extends JsonFormat[ReturnStatement] with DefaultJsonProtocol {

    def write(c: ReturnStatement): JsValue = JsNull

    def read(json: JsValue): ReturnStatement = {
      val fields = json.asJsObject("Unable to decode JSON as ReturnStatement").fields
      if (fields("type").convertTo[String] != "ReturnStatement") {
        throw new RuntimeException("ReturnStatement object expected")
      } else {
        ReturnStatement(fields("expression").convertTo[BaseASTNode])
      }
    }
  }

  implicit object TupleExpressionJsonFormat extends JsonFormat[TupleExpression] with DefaultJsonProtocol {

    def write(c: TupleExpression): JsValue = JsNull

    def read(json: JsValue): TupleExpression = {
      val fields = json.asJsObject("Unable to decode JSON as ReturnStatement").fields
      if (fields("type").convertTo[String] != "TupleExpression") {
        throw new RuntimeException("TupleExpression object expected")
      } else {
        TupleExpression(fields("components").convertTo[List[BaseASTNode]], fields("isArray").convertTo[Boolean])
      }
    }
  }

  implicit object FunctionTypeNameJsonFormat extends JsonFormat[FunctionTypeName] with DefaultJsonProtocol {

    def write(c: FunctionTypeName): JsValue = JsNull

    def read(json: JsValue): FunctionTypeName = {
      val fields = json.asJsObject("Unable to decode JSON as FunctionTypeName").fields
      if (fields("type").convertTo[String] != "FunctionTypeName") {
        throw new RuntimeException("FunctionTypeName object expected")
      } else {
        FunctionTypeName(
          fields("parameterTypes").convertTo[List[BaseASTNode]],
          fields("returnTypes").convertTo[List[BaseASTNode]],
          fields("visibility") match {
            case x : JsString => x.convertTo[String]
            case _ => null
          },
          fields("stateMutability") match {
            case x : JsString => x.convertTo[String]
            case _ => null
          }
        )
      }
    }
  }
  implicit object NewExpressionJsonFormat extends JsonFormat[NewExpression] with DefaultJsonProtocol {

    def write(c: NewExpression): JsValue = JsNull

    def read(json: JsValue): NewExpression = {
      val fields = json.asJsObject("Unable to decode JSON as NewExpression").fields
      if (fields("type").convertTo[String] != "NewExpression") {
        throw new RuntimeException("NewExpression object expected")
      } else {
        NewExpression(
          fields("typeName").convertTo[BaseASTNode]
        )
      }
    }
  }

  implicit object TypeNameExpressionJsonFormat extends JsonFormat[TypeNameExpression] with DefaultJsonProtocol {

    def write(c: TypeNameExpression): JsValue = JsNull

    def read(json: JsValue): TypeNameExpression = {
      val fields = json.asJsObject("Unable to decode JSON as TypeNameExpression").fields
      if (fields("type").convertTo[String] != "TypeNameExpression") {
        throw new RuntimeException("TypeNameExpression object expected")
      } else {
        TypeNameExpression(
          fields("typeName").convertTo[BaseASTNode]
        )
      }
    }
  }

  implicit object InlineAssemblyStatementJsonFormat extends JsonFormat[InlineAssemblyStatement] with DefaultJsonProtocol {

    def write(c: InlineAssemblyStatement): JsValue = JsNull

    def read(json: JsValue): InlineAssemblyStatement = {
      val fields = json.asJsObject("Unable to decode JSON as InlineAssemblyStatement").fields
      if (fields("type").convertTo[String] != "InlineAssemblyStatement") {
        throw new RuntimeException("InlineAssemblyStatement object expected")
      } else {
        InlineAssemblyStatement(
          fields("language") match {
            case x: JsObject => x.convertTo[BaseASTNode]
            case _ => null
          },
          fields("body").convertTo[BaseASTNode]
        )
      }
    }
  }

  implicit object AssemblyBlockJsonFormat extends JsonFormat[AssemblyBlock] with DefaultJsonProtocol {

    def write(c: AssemblyBlock): JsValue = JsNull

    def read(json: JsValue): AssemblyBlock = {
      val fields = json.asJsObject("Unable to decode JSON as AssemblyBlock").fields
      if (fields("type").convertTo[String] != "AssemblyBlock") {
        throw new RuntimeException("AssemblyBlock object expected")
      } else {
        AssemblyBlock(
          fields("operations").convertTo[List[BaseASTNode]]
        )
      }
    }
  }

  implicit object AssemblyAssignmentJsonFormat extends JsonFormat[AssemblyAssignment] with DefaultJsonProtocol {

    def write(c: AssemblyAssignment): JsValue = JsNull

    def read(json: JsValue): AssemblyAssignment = {
      val fields = json.asJsObject("Unable to decode JSON as AssemblyAssignment").fields
      if (fields("type").convertTo[String] != "AssemblyAssignment") {
        throw new RuntimeException("AssemblyAssignment object expected")
      } else {
        AssemblyAssignment(
          fields("names").convertTo[List[BaseASTNode]],
          fields("expression").convertTo[BaseASTNode]
        )
      }
    }
  }

  implicit object AssemblyCallJsonFormat extends JsonFormat[AssemblyCall] with DefaultJsonProtocol {

    def write(c: AssemblyCall): JsValue = JsNull

    def read(json: JsValue): AssemblyCall = {
      val fields = json.asJsObject("Unable to decode JSON as AssemblyCall").fields
      if (fields("type").convertTo[String] != "AssemblyCall") {
        throw new RuntimeException("AssemblyCall object expected")
      } else {
        AssemblyCall(
          fields("functionName").convertTo[String],
          fields("arguments").convertTo[List[BaseASTNode]]
        )
      }
    }
  }

  implicit object DecimalNumberJsonFormat extends JsonFormat[DecimalNumber] with DefaultJsonProtocol {

    def write(c: DecimalNumber): JsValue = JsNull

    def read(json: JsValue): DecimalNumber = {
      val fields = json.asJsObject("Unable to decode JSON as DecimalNumber").fields
      if (fields("type").convertTo[String] != "DecimalNumber") {
        throw new RuntimeException("DecimalNumber object expected")
      } else {
        DecimalNumber(
          fields("value").convertTo[String]
        )
      }
    }
  }

  implicit object ThrowStatementJsonFormat extends JsonFormat[ThrowStatement] with DefaultJsonProtocol {

    def write(c: ThrowStatement): JsValue = JsNull

    def read(json: JsValue): ThrowStatement = {
      val fields = json.asJsObject("Unable to decode JSON as ThrowStatement").fields
      if (fields("type").convertTo[String] != "ThrowStatement") {
        throw new RuntimeException("ThrowStatement object expected")
      } else {
        ThrowStatement()
      }
    }
  }

  implicit object ConditionalJsonFormat extends JsonFormat[Conditional] with DefaultJsonProtocol {

    def write(c: Conditional): JsValue = JsNull

    def read(json: JsValue): Conditional = {
      val fields = json.asJsObject("Unable to decode JSON as Conditional").fields
      if (fields("type").convertTo[String] != "Conditional") {
        throw new RuntimeException("Conditional object expected")
      } else {
        Conditional(
          fields("condition").convertTo[BaseASTNode]
        )
      }
    }
  }

}
