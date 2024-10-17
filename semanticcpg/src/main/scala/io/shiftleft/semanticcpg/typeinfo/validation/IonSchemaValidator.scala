package io.shiftleft.semanticcpg.typeinfo.validation

import com.amazon.ion.{IonSystem, IonValue}
import com.amazon.ion.system.IonSystemBuilder
import com.amazon.ionschema.*

class IonSchemaValidator(rawSchema: String, topLevelType: String) {
  val iss    = IonSchemaSystemBuilder.standard().build()
  val schema = iss.newSchema(rawSchema)
  val typ    = schema.getType(topLevelType)

  def validate(rawIon: String): Violations = {
    val ionSystemBuilder = IonSystemBuilder.standard().build()
    val ionValue         = ionSystemBuilder.singleValue(rawIon)
    val violations       = typ.validate(ionValue)
    violations
  }
}
