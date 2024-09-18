package io.shiftleft.semanticcpg.typeinfo

case class TypeDecl(name: String = "",
                    fullName: String = "",
                    typeParams: List[String] = List(),
                    inherits: List[String] = List(),
                    methods: List[Method] = List(),
                    members: List[Member] = List(),
                    dependencies: List[Dependency] = List())
case class Method(name: String = "", fullName: String = "", signature: String = "")
case class Member(name: String = "", typeFullName: String = "")
case class Dependency(fullName: String = "", version: Option[String] = None)