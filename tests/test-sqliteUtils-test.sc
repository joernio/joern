import java.io.File
import scala.collection.mutable.LinkedHashMap
import java.sql.{Connection, DriverManager, ResultSet}

class SQLiteUtilsTest {
  private val TEST_DB_PATH = "test.db"

  // Test preparation
  def beforeTest(): Unit = {
    SQLiteUtils.connect(TEST_DB_PATH)
  }

  // Test cleanup
  def afterTest(): Unit = {
    SQLiteUtils.close()
    val dbFile = new File(TEST_DB_PATH)
    if (dbFile.exists()) {
      dbFile.delete()
    }
  }

  // Basic table creation test
  def testCreateTableBasic(): Unit = {
    beforeTest()
    println("Starting createTableBasic test")
    val tableName = "test_table"
    val columnNames = List("id", "name")
    val columnTypes = List("INTEGER", "TEXT")
    
    println(s"Creating table $tableName, columns: ${columnNames.mkString(", ")}")
    SQLiteUtils.createTable(tableName, columnNames, columnTypes)
    val resultSet = SQLiteUtils.getTableData(tableName)
    val metaData = resultSet.getMetaData
    
    println(s"Table structure validation: column count=${metaData.getColumnCount}")
    assert(metaData.getColumnCount == 2)
    assert(metaData.getColumnName(1) == "id")
    assert(metaData.getColumnName(2) == "name")
    println("createTableBasic test completed")
    afterTest()
  }

  // Test condition query functionality
  def testDataConditions(): Unit = {
    beforeTest()
    println("Starting condition query test")
    
    // Create test table and insert data
    val tableName = "test_conditions"
    val columnNames = List("id", "name")
    val columnTypes = List("INTEGER", "TEXT")
    SQLiteUtils.createTable(tableName, columnNames, columnTypes)
    
    val testData = List(
      (1, "apple"),
      (2, "banana"),
      (3, "application"),
      (4, "grape")
    )
    SQLiteUtils.insertTupleListToTable(tableName, testData)
    
    // Test like condition
    val likeResult = SQLiteUtils.getDataByConditions(tableName, List(("name", "like", List("app"))))
    assert(likeResult.length == 2)
    
    // Test notLike condition
    val notLikeResult = SQLiteUtils.getDataByConditions(tableName, List(("name", "notLike", List("app"))))
    assert(notLikeResult.length == 2)
    
    // Test startsWith condition
    val startsWithResult = SQLiteUtils.getDataByConditions(tableName, List(("name", "startWith", List("app"))))
    assert(startsWithResult.length == 2)
    
    // Test notStartWith condition
    val notStartWithResult = SQLiteUtils.getDataByConditions(tableName, List(("name", "notStartWith", List("app"))))
    assert(notStartWithResult.length == 2)
    
    println("Condition query test completed")
    afterTest()
  }

  // Complex table creation test
  def testCreateTableComplex(): Unit = {
    beforeTest()
    println("Starting createTableComplex test")
    val tableName = "complex_table"
    val columnNames = List("id", "name", "age", "email", "created_at")
    val columnTypes = List("INTEGER PRIMARY KEY", "TEXT", "INTEGER", "TEXT", "TIMESTAMP")
    
    println(s"Creating complex table $tableName, columns: ${columnNames.mkString(", ")}")
    SQLiteUtils.createTable(tableName, columnNames, columnTypes)
    val resultSet = SQLiteUtils.getTableData(tableName)
    val metaData = resultSet.getMetaData
    
    println(s"Table structure validation: column count=${metaData.getColumnCount}")
    assert(metaData.getColumnCount == 5)
    assert(metaData.getColumnName(4) == "email")
    println("createTableComplex test completed")
    afterTest()
  }

  // Basic list insertion test
  def testInsertListToTableBasic(): Unit = {
    beforeTest()
    println("Starting insertListToTableBasic test")
    val tableName = "test_list"
    val columnName = "value"
    val data = List("apple", "banana", "orange")
    
    println(s"Creating table $tableName and inserting data: ${data.mkString(", ")}")
    SQLiteUtils.createTable(tableName, List(columnName), List("TEXT"))
    SQLiteUtils.insertListToTable(tableName, columnName, data)
    
    val result = SQLiteUtils.getColumnData(tableName, List(columnName))
    println(s"Query result: record count=${result.size}")
    assert(result.size == 3)
    assert(result.head.productElement(0) == "apple")
    println("insertListToTableBasic test completed")
    afterTest()
  }

  // Empty list insertion test
  def testInsertListToTableEmpty(): Unit = {
    beforeTest()
    val tableName = "empty_list"
    val columnName = "value"
    val data = List[String]()
    
    SQLiteUtils.createTable(tableName, List(columnName), List("TEXT"))
    SQLiteUtils.insertListToTable(tableName, columnName, data)
    
    val result = SQLiteUtils.getColumnData(tableName, List(columnName))
    assert(result.size == 0)
    afterTest()
  }

  // Basic condition query test
  def testGetDataByConditionsBasic(): Unit = {
    beforeTest()
    val tableName = "test_conditions"
    SQLiteUtils.createTable(tableName, List("name", "age"), List("TEXT", "INTEGER"))
    
    SQLiteUtils.executeUpdate("INSERT INTO test_conditions VALUES ('Alice', 25)")
    SQLiteUtils.executeUpdate("INSERT INTO test_conditions VALUES ('Bob', 30)")
    
    val conditions = List(("age", "=", List(25)))
    val result = SQLiteUtils.getDataByConditions(tableName, conditions)
    
    assert(result.size == 1)
    assert(result.head("name") == "Alice")
    afterTest()
  }

  // Complex condition query test
  def testGetDataByConditionsComplex(): Unit = {
    beforeTest()
    println("Starting getDataByConditionsComplex test")
    val tableName = "complex_conditions"
    SQLiteUtils.createTable(tableName, List("name", "description"), List("TEXT", "TEXT"))
    
    println("Inserting test data")
    SQLiteUtils.executeUpdate("INSERT INTO complex_conditions VALUES ('test1', 'This is a test')")
    SQLiteUtils.executeUpdate("INSERT INTO complex_conditions VALUES ('test2', 'Another test case')")
    SQLiteUtils.executeUpdate("INSERT INTO complex_conditions VALUES ('other', 'Different content')")
    
    val conditions = List(
      ("description", "like", List("test")),
      ("name", "!=", List("test2"))
    )
    println(s"Executing complex condition query: ${conditions}")
    val result = SQLiteUtils.getDataByConditions(tableName, conditions)
    
    println(s"Query result: record count=${result.size}")
    assert(result.size == 1)
    assert(result.head("name") == "test1")
    println("getDataByConditionsComplex test completed")
    afterTest()
  }

  // Basic tuple list insertion test
  def testInsertTupleListToTableBasic(): Unit = {
    beforeTest()
    val tableName = "test_tuples"
    SQLiteUtils.createTable(tableName, List("id", "name", "age"), List("INTEGER", "TEXT", "INTEGER"))
    
    val data = List(
      Tuple3(1, "Alice", 25),
      Tuple3(2, "Bob", 30)
    )
    
    SQLiteUtils.insertTupleListToTable(tableName, data)
    val result = SQLiteUtils.getColumnData(tableName, List("id", "name", "age"))
    
    assert(result.size == 2)
    assert(result.head.productElement(0) == 1)
    assert(result.last.productElement(1) == "Bob")
    afterTest()
  }

  // Empty tuple list insertion test
  def testInsertTupleListToTableEmpty(): Unit = {
    beforeTest()
    val tableName = "empty_tuples"
    SQLiteUtils.createTable(tableName, List("id", "name"), List("INTEGER", "TEXT"))
    
    val data = List[Tuple2[Int, String]]()
    SQLiteUtils.insertTupleListToTable(tableName, data)
    
    val result = SQLiteUtils.getColumnData(tableName, List("id", "name"))
    assert(result.size == 0)
    afterTest()
  }

  // BasicMap sequence insertion test
  def testInsertMapSeqToTableBasic(): Unit = {
    beforeTest()
    val tableName = "test_maps"
    val data = Seq(
      LinkedHashMap("name" -> "Alice", "age" -> "25"),
      LinkedHashMap("name" -> "Bob", "age" -> "30")
    )
    
    SQLiteUtils.insertMapSeqToTable(tableName, data)
    val result = SQLiteUtils.getColumnData(tableName, List("name", "age"))
    
    assert(result.size == 2)
    assert(result.head.productElement(0) == "Alice")
    assert(result.last.productElement(1) == "30")
    afterTest()
  }

  // Empty Map sequence insertion test
  def testInsertMapSeqToTableEmpty(): Unit = {
    beforeTest()
    val tableName = "empty_maps"
    val data = Seq[LinkedHashMap[String, String]]()
    
    SQLiteUtils.insertMapSeqToTable(tableName, data)
    
    val tableExists = SQLiteUtils.tableExists(tableName)
    assert(!tableExists)
    afterTest()
  }

  // Test column selection functionality of getDataByConditions method
  def testGetDataByConditionsWithColumns(): Unit = {
    beforeTest()
    println("Starting getDataByConditionsWithColumns test")
    val tableName = "test_columns"
    SQLiteUtils.createTable(tableName, List("id", "name", "age"), List("INTEGER", "TEXT", "INTEGER"))
    
    println("Inserting test data")
    SQLiteUtils.executeUpdate("INSERT INTO test_columns VALUES (1, 'Alice', 25)")
    SQLiteUtils.executeUpdate("INSERT INTO test_columns VALUES (2, 'Bob', 30)")
    SQLiteUtils.executeUpdate("INSERT INTO test_columns VALUES (3, 'Charlie', 35)")
    
    // Test empty list returns all columns
    println("Testing empty list returns all columns")
    val allColumnsResult = SQLiteUtils.getDataByConditions(tableName, List(), List())
    assert(allColumnsResult.size == 3)
    assert(allColumnsResult.head.size == 3)
    assert(allColumnsResult.head.contains("id"))
    assert(allColumnsResult.head.contains("name"))
    assert(allColumnsResult.head.contains("age"))
    
    // Test single column query
    println("Testing single column query")
    val singleColumnResult = SQLiteUtils.getDataByConditions(tableName, List(), List("name"))
    assert(singleColumnResult.size == 3)
    assert(singleColumnResult.head.size == 1)
    assert(singleColumnResult.head.contains("name"))
    assert(!singleColumnResult.head.contains("age"))
    
    // Test multiple columns query
    println("Testing multiple columns query")
    val multiColumnsResult = SQLiteUtils.getDataByConditions(tableName, List(("age", ">", List(25))), List("name", "age"))
    assert(multiColumnsResult.size == 2)
    assert(multiColumnsResult.head.size == 2)
    assert(multiColumnsResult.head.contains("name"))
    assert(multiColumnsResult.head.contains("age"))
    assert(!multiColumnsResult.head.contains("id"))
    
    println("getDataByConditionsWithColumns test completed")
    afterTest()
  }

  // Run all tests
  def runAllTests(): Unit = {
    testCreateTableBasic()
    testCreateTableComplex()
    testInsertListToTableBasic()
    testInsertListToTableEmpty()
    testGetDataByConditionsBasic()
    testGetDataByConditionsComplex()
    testGetDataByConditionsWithColumns()
    testInsertTupleListToTableBasic()
    testInsertTupleListToTableEmpty()
    testInsertMapSeqToTableBasic()
    testInsertMapSeqToTableEmpty()
    println("All tests completed")
  }
}

// Create test instance and run tests
val test = new SQLiteUtilsTest()
println("Running tests in shell:")
println("joern --script test-sqliteUtils-test.sc --import test-sqliteUtils.sc")
println("Running tests in cli:")
println(":load test-sqliteUtils.sc")
println(":load test-sqliteUtils-test.sc")
test.runAllTests()