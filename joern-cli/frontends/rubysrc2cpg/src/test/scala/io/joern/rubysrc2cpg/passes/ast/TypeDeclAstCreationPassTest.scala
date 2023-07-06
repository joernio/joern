package io.joern.rubysrc2cpg.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.semanticcpg.language._

class TypeDeclAstCreationPassTest extends RubyCode2CpgFixture {

  "AST generation for simple classes declarations" should {

    "generate a basic type declaration node for an empty class" in {
      val cpg = code("""
          |class MyClass
          |end
          |""".stripMargin)
      val Some(myClass) = cpg.typeDecl.nameExact("MyClass").headOption: @unchecked
      myClass.name shouldBe "MyClass"
      myClass.fullName shouldBe "Test0.rb::program:MyClass"
    }

    "generate methods under type declarations" in {
      val cpg = code("""
          |class Vehicle
          |
          |   def self.speeding
          |     "Hello, from a class method"
          |   end
          |
          |   def Vehicle.halting
          |     "Hello, from another class method"
          |   end
          |
          |   def driving
          |     "Hello, from an instance method"
          |   end
          |
          |end
          |""".stripMargin)
      val Some(vehicle) = cpg.typeDecl.nameExact("Vehicle").headOption: @unchecked
      vehicle.name shouldBe "Vehicle"
      vehicle.fullName shouldBe "Test0.rb::program:Vehicle"

      val List(speeding, halting, driving) = vehicle.method.l
      speeding.name shouldBe "speeding"
      halting.name shouldBe "halting"
      driving.name shouldBe "driving"

      speeding.fullName shouldBe "Test0.rb::program:Vehicle:speeding"
      halting.fullName shouldBe "Test0.rb::program:Vehicle:halting"
      driving.fullName shouldBe "Test0.rb::program:Vehicle:driving"
    }

    "generate members for various class members under the respective type declaration" ignore {
      val cpg = code("""
          |class Song
          |  @@plays = 0
          |  def initialize(name, artist, duration)
          |    @name     = name
          |    @artist   = artist
          |    @duration = duration
          |  end
          |end
          |""".stripMargin)
      val Some(song) = cpg.typeDecl.nameExact("Song").headOption: @unchecked
      song.name shouldBe "Song"
      song.fullName shouldBe "Test0.rb::program:Song"

      val List(plays, name, artist, duration) = song.member.l

      plays.name shouldBe "plays"
      name.name shouldBe "name"
      artist.name shouldBe "artist"
      duration.name shouldBe "duration"
    }

    "generate members for various class members when using the `attr_reader` and `attr_writer` idioms" ignore {
      val cpg = code("""
          |class Song
          |  attr_reader :name, :artist, :duration
          |  attr_writer :album
          |end
          |""".stripMargin)
      val Some(song) = cpg.typeDecl.nameExact("Song").headOption: @unchecked
      song.name shouldBe "Song"
      song.fullName shouldBe "Test0.rb::program:Song"

      val List(name, artist, duration, album) = song.member.l

      name.name shouldBe "name"
      artist.name shouldBe "artist"
      duration.name shouldBe "duration"
      album.name shouldBe "album"
    }

    "generate methods with the correct access control modifiers case 1" in {
      val cpg = code("""
          |class MyClass
          |
          |    def method1    # default is 'public'
          |      #...
          |    end
          |
          |  protected          # subsequent methods will be 'protected'
          |
          |    def method2    # will be 'protected'
          |      #...
          |    end
          |
          |  private            # subsequent methods will be 'private'
          |
          |    def method3    # will be 'private'
          |      #...
          |    end
          |
          |  public             # subsequent methods will be 'public'
          |
          |    def method4    # and this will be 'public'
          |      #...
          |    end
          |end
          |""".stripMargin)
      val Some(myClass) = cpg.typeDecl.nameExact("MyClass").headOption: @unchecked
      myClass.name shouldBe "MyClass"
      myClass.fullName shouldBe "Test0.rb::program:MyClass"

      val List(m1, m2, m3, m4) = myClass.method.l
      m1.name shouldBe "method1"
      m2.name shouldBe "method2"
      m3.name shouldBe "method3"
      m4.name shouldBe "method4"

      m1.fullName shouldBe "Test0.rb::program:MyClass:method1"
      m2.fullName shouldBe "Test0.rb::program:MyClass:method2"
      m3.fullName shouldBe "Test0.rb::program:MyClass:method3"
      m4.fullName shouldBe "Test0.rb::program:MyClass:method4"

      m1.modifier.modifierType.l shouldBe List(ModifierTypes.PUBLIC)
      m2.modifier.modifierType.l shouldBe List(ModifierTypes.PROTECTED)
      m3.modifier.modifierType.l shouldBe List(ModifierTypes.PRIVATE)
      m4.modifier.modifierType.l shouldBe List(ModifierTypes.PUBLIC)
    }

    "generate methods with the correct access control modifiers case 2" ignore {
      val cpg = code("""
          |class MyClass
          |
          |  def method1
          |  end
          |
          |  def method2
          |  end
          |
          |  def method3
          |  end
          |
          |  def method4
          |  end
          |
          |  public    :method1, :method4
          |  protected :method2
          |  private   :method3
          |end
          |""".stripMargin)
      val Some(myClass) = cpg.typeDecl.nameExact("MyClass").headOption: @unchecked
      myClass.name shouldBe "MyClass"
      myClass.fullName shouldBe "Test0.rb::program:MyClass"

      val List(m1, m2, m3, m4) = myClass.method.l
      m1.name shouldBe "method1"
      m2.name shouldBe "method2"
      m3.name shouldBe "method3"
      m4.name shouldBe "method4"

      m1.fullName shouldBe "Test0.rb::program:MyClass:method1"
      m2.fullName shouldBe "Test0.rb::program:MyClass:method2"
      m3.fullName shouldBe "Test0.rb::program:MyClass:method3"
      m4.fullName shouldBe "Test0.rb::program:MyClass:method4"

      m1.modifier.modifierType.l shouldBe List(ModifierTypes.PUBLIC)
      m2.modifier.modifierType.l shouldBe List(ModifierTypes.PROTECTED)
      m3.modifier.modifierType.l shouldBe List(ModifierTypes.PRIVATE)
      m4.modifier.modifierType.l shouldBe List(ModifierTypes.PUBLIC)
    }

  }

  "Polymorphism in classes" should {

    "correctly contain the inherited base type name in the super type" ignore {
      val cpg = code("""
          |class GeeksforGeeks
          |    def initialize
          |        puts "This is Superclass"
          |    end
          |     
          |    def super_method
          |        puts "Method of superclass"
          |    end
          |end
          | 
          |class Sudo_Placement < GeeksforGeeks
          |    def initialize
          |       puts "This is Subclass"
          |    end
          |end
          |""".stripMargin)

      val Some(baseType) = cpg.typeDecl.nameExact("GeeksforGeeks").headOption: @unchecked
      baseType.name shouldBe "GeeksforGeeks"
      baseType.fullName shouldBe "Test0.rb::program:GeeksforGeeks"

      val Some(subType) = cpg.typeDecl.nameExact("Sudo_Placement").headOption: @unchecked
      subType.name shouldBe "Sudo_Placement"
      subType.fullName shouldBe "Test0.rb::program:Sudo_Placement"
      subType.inheritsFromTypeFullName shouldBe Seq("Test0.rb::program:GeeksforGeeks")
    }

  }

}
