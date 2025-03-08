import java.sql.{Connection, DriverManager, ResultSet}
import scala.collection.mutable.{Set, LinkedHashMap}

/**
 * SQLiteUtils - A utility object for SQLite database operations for joern script
 *
 * Usage Examples:
 * more usage can find in test-sqliteUtils-test.sc
 * 1. Basic Database Operations:
 *    // Connect to database
 *    SQLiteUtils.connect("mydb.db")
 *
 *    // Create a table
 *    SQLiteUtils.createTable("users", List("id", "name", "age"), List("INTEGER", "TEXT", "INTEGER"))
 *
 *    // Close connection when done
 *    SQLiteUtils.close()
 *
 * 2. Data Insertion:
 *    // Insert single column data
 *    val fruits = List("apple", "banana", "orange")
 *    SQLiteUtils.insertListToTable("fruits", "name", fruits)
 *
 *    // Insert tuple data
 *    val users = List((1, "John", 25), (2, "Alice", 30))
 *    SQLiteUtils.insertTupleListToTable("users", users)
 *
 *    // Insert map data
 *    val products = Seq(
 *      LinkedHashMap("id" -> "1", "name" -> "Laptop", "price" -> "999"),
 *      LinkedHashMap("id" -> "2", "name" -> "Phone", "price" -> "599")
 *    )
 *    SQLiteUtils.insertMapSeqToTable("products", products)
 *
 * 3. Data Retrieval:
 *    // Get all data from table
 *    val allUsers = SQLiteUtils.getTableData("users")
 *
 *    // Get specific columns
 *    val names = SQLiteUtils.getColumnData("users", List("name", "age"))
 *
 * 4. Conditional Queries:
 *    // Search with various conditions
 *    val conditions = List(
 *      ("age", ">", List(25)),
 *      ("name", "like", List("Jo"))
 *    )
 *    val filteredUsers = SQLiteUtils.getDataByConditions("users", conditions)
 *
 * 5. Table Management:
 *    // Check if table exists
 *    val exists = SQLiteUtils.tableExists("users")
 *
 *    // Execute custom SQL
 *    SQLiteUtils.executeUpdate("DROP TABLE IF EXISTS users")
 */

object SQLiteUtils {
  private var connection: Connection = null
  private var dbPath: String = null

  // Initialize database connection
  def connect(path: String): Unit = {
    dbPath = path
    Class.forName("org.sqlite.JDBC")
    connection = DriverManager.getConnection(s"jdbc:sqlite:$dbPath")
  }

  // Close database connection
  def close(): Unit = {
    if (connection != null && !connection.isClosed) {
      connection.close()
    }
  }

  // Get all data from the specified table
  def getTableData(tableName: String): ResultSet = {
    val statement = connection.createStatement()
    statement.executeQuery(s"SELECT * FROM $tableName")
  }

  // Get data from specified table and columns and convert to tuple list
  def getColumnData[T](tableName: String, columnNames: List[String]): List[Tuple] = {
    val result = scala.collection.mutable.ListBuffer[Tuple]()
    val statement = connection.createStatement()
    val columns = columnNames.mkString(", ")
    val resultSet = statement.executeQuery(s"SELECT $columns FROM $tableName")

    while (resultSet.next()) {
      // Build tuple
      val tuple = columnNames.map { columnName =>
        resultSet.getObject(columnName)
      }.toArray
      result.append(Tuple.fromArray(tuple))
    }

    resultSet.close()
    statement.close()
    result.toList
  }

  // Create table
  def createTable(tableName: String, columnNames: List[String], columnTypes: List[String]): Unit = {
    val columns = columnNames.zip(columnTypes).map { case (name, dataType) => s"$name $dataType" }.mkString(", ")
    val sql = s"CREATE TABLE IF NOT EXISTS $tableName ($columns)"
    val statement = connection.createStatement()
    statement.executeUpdate(sql)
    statement.close()
  }

  // Insert List data into specified table
  def insertListToTable[T](tableName: String, columnName: String, data: List[T]): Unit = {
    val statement = connection.createStatement()
    data.foreach { value =>
      val sql = s"INSERT INTO $tableName ($columnName) VALUES ('$value')"
      statement.executeUpdate(sql)
    }
    statement.close()
  }

  // Insert tuple list data into specified table
  def insertTupleListToTable[T <: Product](tableName: String, data: List[T]): Unit = {
    if (data.isEmpty) return
    val statement = connection.createStatement()
    data.foreach { tuple =>
      val values = (1 to tuple.productArity).map(i => s"'${tuple.productElement(i-1)}'")
      val sql = s"INSERT INTO $tableName VALUES (${values.mkString(", ")})"
      statement.executeUpdate(sql)
    }
    statement.close()
  }

  // Insert Map sequence data into specified table
  def insertMapSeqToTable(tableName: String, data: Seq[LinkedHashMap[String, String]]): Unit = {
    if (data.isEmpty) return
    val columnNames = data.head.keys.toList
    createTable(tableName, columnNames, List.fill(columnNames.length)("TEXT"))
    
    val statement = connection.createStatement()
    data.foreach { row =>
      val values = columnNames.map(col => s"'${row(col)}'")
      val sql = s"INSERT INTO $tableName (${columnNames.mkString(", ")}) VALUES (${values.mkString(", ")})"
      statement.executeUpdate(sql)
    }
    statement.close()
  }

  // Get data by conditions
  def getDataByConditions(tableName: String, conditions: List[(String, String, List[Any])], columns: List[String] = List()): List[Map[String, Any]] = {
    val result = scala.collection.mutable.ListBuffer[Map[String, Any]]()
    val whereClause = conditions.map {
      case (column, "=", values) => s"$column = '${values.head}'"
      case (column, "!=", values) => s"$column != '${values.head}'"
      case (column, ">", values) => s"$column > '${values.head}'"
      case (column, ">=", values) => s"$column >= '${values.head}'"
      case (column, "<", values) => s"$column < '${values.head}'"
      case (column, "<=", values) => s"$column <= '${values.head}'"
      case (column, "like", values) => s"$column LIKE '%${values.head}%'"
      case (column, "notLike", values) => s"$column NOT LIKE '%${values.head}%'"
      case (column, "startWith", values) => s"$column LIKE '${values.head}%'"
      case (column, "notStartWith", values) => s"$column NOT LIKE '${values.head}%'"
      case _ => ""
    }.filter(_.nonEmpty).mkString(" AND ")

    val selectColumns = if (columns.isEmpty) "*" else columns.mkString(", ")
    val sql = s"SELECT $selectColumns FROM $tableName${if (whereClause.nonEmpty) s" WHERE $whereClause" else ""}"
    val statement = connection.createStatement()
    val resultSet = statement.executeQuery(sql)
    val metaData = resultSet.getMetaData
    val columnCount = metaData.getColumnCount

    while (resultSet.next()) {
      val row = (1 to columnCount).map { i =>
        val columnName = metaData.getColumnName(i)
        columnName -> resultSet.getObject(i)
      }.toMap
      result.append(row)
    }

    resultSet.close()
    statement.close()
    result.toList
  }

  // Execute update operation
  def executeUpdate(sql: String): Int = {
    val statement = connection.createStatement()
    val result = statement.executeUpdate(sql)
    statement.close()
    result
  }

  // Check if table exists
  def tableExists(tableName: String): Boolean = {
    val statement = connection.createStatement()
    try {
      statement.executeQuery(s"SELECT 1 FROM $tableName LIMIT 1")
      true
    } catch {
      case _: Exception => false
    } finally {
      statement.close()
    }
  }
}