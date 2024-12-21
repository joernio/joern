package io.joern.scanners.php

import io.joern.console.*
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.macros.QueryMacros.*
import io.joern.scanners.*
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

object TwigTemplateInjection extends QueryBundle {

  implicit val resolver: ICallResolver = NoResolve

  @q
  def TwigTemplateInjection()(implicit context: EngineContext): Query =
    Query.make(
      name = "twig-template-injection",
      author = Crew.SJ1iu,
      title = "Twig-Template-Injection: A parameter controlled by the user is rendered within a Twig template.",
      description = """
          |An attacker controlled parameter is used in an twig template.
          |
          |This doesn't necessarily indicate a Twig template injection, but if the input is not sanitized and the escape settings are disabled in the application, it could potentially lead to a template injection vulnerability.
          |""".stripMargin,
      score = 5,
      withStrRep({ cpg =>

        def source =
          cpg.call.name(Operators.assignment).argument.code("(?i).*request.*")

        def sink =
          cpg.call.name("createTemplate").methodFullName("(?i).*twig.*").argument

        sink.reachableBy(source).iterator

      }),
      tags = List(QueryTags.remoteCodeExecution, QueryTags.default),
      multiFileCodeExamples = MultiFileCodeExamples(
        positive = List(
          List(
            CodeSnippet(
              """
      |<?php 
      |require_once 'vendor/autoload.php'; // Load Composer autoloader
      |
      |use Twig\Loader\FilesystemLoader;
      |use Twig\Environment;
      |
      |// Set up Twig environment
      |$loader = new FilesystemLoader('./Template'); // Directory for templates
      |$twig = new Environment($loader, [
      |    'cache' => false, // Disable caching for development
      |    'debug' => true,  // Enable debugging
      |    'autoescape' => false // Disabling auto-escaping can lead to template injection vulnerabilities, potentially allowing command execution if the input is not properly sanitized
      |]);
      |
      |// Get the 'name' parameter from the request, some other dummy parameters are provided but not in use. The rule will only detect the vulnerable parameter "name". The rule is granular enough to detect other requests, such as those using Symfony\Component\HttpFoundation\Request.
      |$name = $_REQUEST['name'] ?? 'Guest';
      |$name2 = $_REQUEST['name2'] ?? 'Guest';
      |$name3 = $_REQUEST['name3'] ?? 'Guest';
      |$name4 = $_REQUEST['name4'] ?? 'Guest';
      |$name5 = $_REQUEST['name5'] ?? 'Guest';
      |
      |// Render a dynamic template using createTemplate. This is the sink.
      |$template = $twig->createTemplate("Hello, {$name}! Welcome to Twig dynamic templates.");
      |
      |// Render
      |echo $template->render(['name' => $name]);
      |""".stripMargin,
              "Positive.kt"
            )
          )
        ),
        negative = List(
          List(
            CodeSnippet(
              """
      |<?php
      |require_once 'vendor/autoload.php'; // Load Composer autoloader
      |
      |use Twig\Loader\FilesystemLoader;
      |use Twig\Environment;
      |
      |// Set up Twig environment
      |$loader = new FilesystemLoader('./Template'); // Directory for templates
      |$twig = new Environment($loader, [
      |    'cache' => false, // Disable caching for development
      |    'debug' => true,  // Enable debugging
      |    'autoescape' => false
      |]);
      |
      |// This time a custom function named "createTemplate" is defined which has no template injection issues. It's simply echo the user's input.
      |function createTemplate($templateString) {
      |    echo $templateString;
      |}
      |
      |// Get the 'name' parameter from the request, some other dummy parameters are provided but not in use.
      |$name = $_REQUEST['name'] ?? 'Guest';
      |$name2 = $_REQUEST['name2'] ?? 'Guest';
      |$name3 = $_REQUEST['name3'] ?? 'Guest';
      |$name4 = $_REQUEST['name4'] ?? 'Guest';
      |$name5 = $_REQUEST['name5'] ?? 'Guest';
      |
      |// All Twig functions below are commented out, but the custom "createTemplate" function is called. No Twig template injection occurred this time, and the rule will not report an issue even if the function names "createTemplate" is invoked.
      |createTemplate($name);
      |
      |// Render a regular template
      |// echo $twig->render('template.twig', ['title' => 'Twig Setup', 'name' => $name]);
      |
      |// Render a dynamic template using createTemplate
      |// $template = $twig->createTemplate("Hello, {$name}! Welcome to Twig dynamic templates.");
      |
      |// echo $template->render(['name' => $name]);
      |""".stripMargin,
              "Negative.kt"
            )
          )
        )
      )
    )
}
