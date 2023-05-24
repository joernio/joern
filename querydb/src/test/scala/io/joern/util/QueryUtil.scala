package io.joern.util

import io.joern.suites.QDBArgumentProvider
import io.joern.querydb.{QueryBundle, QueryDatabase}
import io.joern.querydb.Query

object QueryUtil {
  object EmptyBundle extends QueryBundle

  def allQueries(queryBundle: QueryBundle, argumentProvider: QDBArgumentProvider): List[Query] = {
    new QueryDatabase(defaultArgumentProvider = argumentProvider).queriesInBundle(queryBundle.getClass)
  }

}
