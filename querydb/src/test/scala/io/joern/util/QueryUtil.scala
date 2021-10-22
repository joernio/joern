package io.joern.util

import io.joern.suites.QDBArgumentProvider
import io.shiftleft.console.{Query, QueryBundle, QueryDatabase}

object QueryUtil {
  object EmptyBundle extends QueryBundle

  def allQueries(queryBundle: QueryBundle, argumentProvider: QDBArgumentProvider): List[Query] = {
    new QueryDatabase(defaultArgumentProvider = argumentProvider).queriesInBundle(queryBundle.getClass)
  }

}
