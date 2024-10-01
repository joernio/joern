package io.shiftleft.semanticcpg.typeinfo.loading

import com.amazon.ion.IonSystem
import com.amazon.ion.IonValue
import com.amazon.ion.system.IonSystemBuilder
import com.amazon.ionschema.IonSchemaSystemBuilder
import com.amazon.ionschema.IonSchemaSystem
import com.amazon.ionschema.Schema
import com.amazon.ionschema.Type
import com.amazon.ionschema.Violations

class IonSchemaValidator(rawSchema: String, topLevelType: String) {
  val iss = IonSchemaSystemBuilder.standard().build()
  val schema = iss.newSchema(rawSchema)
  val typ = schema.getType(topLevelType)
  
  def validate(rawIon: String): Violations = {
    val ionSystemBuilder = IonSystemBuilder.standard().build()
    val ionValue = ionSystemBuilder.singleValue(rawIon)
    val violations = typ.validate(ionValue)
    violations
  }
}
