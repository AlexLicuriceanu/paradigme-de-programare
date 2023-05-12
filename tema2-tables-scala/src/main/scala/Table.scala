import util.Util.Row
import util.Util.Line
import TestTables.{table3_4_merged, tableFunctional, tableImperative, tableObjectOriented}

trait FilterCond {
  def &&(other: FilterCond): FilterCond = And(this, other)
  def ||(other: FilterCond): FilterCond = Or(this, other)

  // fails if the column name is not present in the row
  def eval(r: Row): Option[Boolean]
}
case class Field(colName: String, predicate: String => Boolean) extends FilterCond {
  // 2.2.
  override def eval(r: Row): Option[Boolean] = {
    r.get(colName).orElse(None).map(predicate)
  }
}

case class And(f1: FilterCond, f2: FilterCond) extends FilterCond {
  // 2.2.
  override def eval(r: Row): Option[Boolean] = {

    // Evaluate first condition.
    f1.eval(r).flatMap(result => {
      if (result) {

        // Check if there are non existent columns in f2.
        val check: Option[Boolean] = f2.eval(r)

        if (check.isEmpty) {
          None
        }
        else {
          // First condition is satisfied, evaluate second condition.
          f2.eval(r)
        }
      }
      else {
        // First condition is not satisfied, return false.
        Some(false)
      }
    })
  }
}

case class Or(f1: FilterCond, f2: FilterCond) extends FilterCond {
  // 2.2.
  override def eval(r: Row): Option[Boolean] = {

    // Evaluate first condition.
    f1.eval(r).flatMap(result => {
      if (result) {
        // Check if there are non existent columns in f2.
        val check: Option[Boolean] = f2.eval(r)

        if (check.isEmpty) {
          None
        }
        else {
          // One condition being true is enough, return true.
          Some(true)
        }
      }
      else {
        // First condition not satisfied, evaluate the second condition.
        f2.eval(r)
      }
    })
  }
}

trait Query {
  def eval: Option[Table]
}

/*
  Atom query which evaluates to the input table
  Always succeeds
 */
case class Value(t: Table) extends Query {
  override def eval: Option[Table] = Some(t)
}

/*
  Selects certain columns from the result of a target query
  Fails with None if some rows are not present in the resulting table
 */
case class Select(columns: Line, target: Query) extends Query {
  override def eval: Option[Table] = {
    target.eval match {
      case Some(table) => table.select(columns)
      case None => None
    }
  }
}

/*
  Filters rows from the result of the target query
  Success depends only on the success of the target
 */
case class Filter(condition: FilterCond, target: Query) extends Query {
  override def eval: Option[Table] = {
    target.eval match {
      case Some(table) => table.filter(condition)
      case None => None
    }
  }
}

/*
  Creates a new column with default values
  Success depends only on the success of the target
 */
case class NewCol(name: String, defaultVal: String, target: Query) extends Query {
  override def eval: Option[Table] = {
    target.eval.map(table => table.newCol(name, defaultVal))
  }
}

/*
  Combines two tables based on a common key
  Success depends on whether the key exists in both tables or not AND on the success of the target
 */
case class Merge(key: String, t1: Query, t2: Query) extends Query {
  override def eval: Option[Table] = {
    t1.eval.flatMap(
      table1 => t2.eval match {
        case Some(table2) => table1.merge(key, table2)
        case None => Some(table1)
      }
    )
  }
}


class Table (columnNames: Line, tabular: List[List[String]]) {
  def getColumnNames : Line = columnNames
  def getTabular : List[List[String]] = tabular

  // 1.1
  override def toString: String = {

    def op(delim: Char)(e: String, acc: String): String = {
      acc match {
        case "" => e
        case _ => e ++ List(delim).mkString ++ acc
      }
    }

    // Handle the columns row then the data rows and concatenate.
    val csvTable: String =
      columnNames.foldRight("")(op(',')) ++ List("\n").mkString ++
      tabular.foldRight("")((line, acc) => line.foldRight("")(op(',')) ++ List("\n").mkString ++ acc)

    // Remove the trailing newline.
    csvTable.slice(0, csvTable.length-1)
  }

  // 2.1
  def select(columns: Line): Option[Table] = {

    // Check if the column exists
    val isColumnExistent = columns.foldRight(true)((col, acc) => columnNames.contains(col) && acc)

    // Check if column exists or return None.
    if (isColumnExistent) {

      // Columns exist, get each column's index.
      val indexes: List[Int] = columns.foldRight(Nil: List[Int])(
        (column, acc) => columnNames.indexOf(column) :: acc)

      // Traverse the table's lines. For each line traverse the index list and
      // create a new list with all elements at the index positions.
      val selected = tabular.foldRight(Nil: List[List[String]])(
        (line, acc) => {
          indexes.foldRight(Nil: List[String])((index, acc) => line(index) :: acc) :: acc
        })

      // Return.
      Some(new Table(columns, selected))
    } else {

      // One or more columns don't exist, return None.
      None
    }
  }

  // 2.2
  def filter(cond: FilterCond): Option[Table] = {

    val filteredRows = tabular.filter(row => {
      // Transform the current row to a map.
      val rowMap = rowToRowMap(columnNames, row)
      // Evaluate the condition on the current row.
      cond.eval(rowMap).getOrElse(false)
    })

    if (filteredRows.isEmpty) {
      None
    } else {
      Some(new Table(columnNames, filteredRows))
    }
  }

  // Helper method to convert line and row data into a row map.
  private def rowToRowMap(columnNames: Line, row: List[String]): Row = {
    columnNames.zip(row).toMap
  }

  // 2.3.
  def newCol(name: String, defaultVal: String): Table = {

    // Check if the column already exists.
    if (columnNames.contains(name)) {
      this
    }
    else {

      // Add the new column's name.
      val newColumnNames: Line = columnNames :+ name

      // Add the default value.
      val newTabular: List[List[String]] = tabular.map(_ :+ defaultVal)

      // Return a new table.
      new Table(newColumnNames, newTabular)
    }
  }

  // 2.4.
  def merge(key: String, other: Table): Option[Table] = {

    // Check that the key is valid.
    if (!this.getColumnNames.contains(key) || !other.getColumnNames.contains(key)) {

      // Key can't be found in one of the tables, return None.
      None
    }
    else {

      // The column names from the first table.
      val table1ColumnNames = columnNames

      // Traverse the other table's column names and if there is
      // a column name that is in the second table and not in the
      // first, add it to newColumnNames.
      val table2ColumnNames = other.getColumnNames.foldRight(Nil: Line)(
        (columnName, acc) => {
          if (table1ColumnNames.contains(columnName)) {
            acc
          }
          else {
            acc :+ columnName
          }
        }
      )

      // Create the new list of column names.
      val newColumnNames: Line = table1ColumnNames ++ table2ColumnNames

      // Get the index of the key in the first table.
      val keyIndex = columnNames.indexOf(key)
      // Get the index of the key in the second table.
      val otherKeyIndex = other.getColumnNames.indexOf(key)


      // Traverse each row of the first table.
      val newTabular1 = tabular.map(row => {

        // Get the entry in the key's column in the first table.
        val keyVal = row(keyIndex)

        // Get the matching row in the other table.
        val matchingRow = other.getTabular.filter(otherRow => otherRow(otherKeyIndex) == keyVal)

        if (matchingRow.nonEmpty) {

          // A matching row was found.

          // This will compute the new row only with the common elements merged.
          val commonEntries = row.map(entry => {

            // Get the index of the entry in the current row.
            val entryIndex = row.indexOf(entry)
            // Get the index of the current row.
            val rowIndex = tabular.indexOf(row)
            // Get the name of the entry's column
            val columnName = columnNames(entryIndex)

            // Check if there is a column with the same name in the other table.
            if (other.getColumnNames.contains(columnName)) {

              // Get the index of the matched row's common column.
              val otherColumnIndex = other.getColumnNames.indexOf(columnName)
              // Get the index of the matched row.
              val otherRowIndex = other.getTabular.indexOf(matchingRow.head)
              // Get the value of the common column in the other table.
              val otherEntry = other.getTabular(otherRowIndex)(otherColumnIndex)

              // Check if the entry is exactly the same.
              if (entry == otherEntry) {

                // Same entries, return only one.
                entry
              }
              else {

                // Entries are different, concatenate with ";".
                entry + ";" + otherEntry
              }
            }
            else {

              // No matching column found.
              entry
            }
          })

          // Common entries have been added, now compute the remaining uncommon columns.
          val otherEntries = table2ColumnNames.foldRight(Nil: List[String])(
            (columnName, acc) => {

              // Get the index of the matched row.
              val otherRowIndex = other.getTabular.indexOf(matchingRow.head)
              // Get the index of the current column.
              val otherColumnIndex = other.getColumnNames.indexOf(columnName)
              // Get the entry and add it to the accumulator.
              other.getTabular(otherRowIndex)(otherColumnIndex) :: acc
            }
          )

          commonEntries ++ otherEntries
        }
        else {

          // No matching row found, add "" for the other table's columns.
          row ++ List.fill(table2ColumnNames.length)("")
        }
      })

      // Lastly, handle the entries that only exist in the other table.

      // Find if there is a match in the first table for the current row from the other table.
      // We are only looking for rows in the second table that don't match ones in the first,
      // since we have already merged the common rows.
      val unmatchedRows = other.getTabular.filterNot(
        otherRow => tabular.exists(row => row(keyIndex) == otherRow(otherKeyIndex)))


      val newTabular2 = unmatchedRows.foldRight(Nil: List[List[String]])(
        (row, acc) => {

          // Get the index of the current row in the other table.
          val otherRowIndex = other.getTabular.indexOf(row)

          // Traverse the merged column names, attribute the values of the corresponding columns
          // and place "" for columns that only exist in the first table.
          val newRow = newColumnNames.foldRight(Nil: List[String])(
            (columnName, acc) => {

              // Check if the column exists in the other table.
              if (other.getColumnNames.contains(columnName)) {

                // Column exists, get the value at position (otherRowIndex, otherColumnIndex).
                val otherColumnIndex = other.getColumnNames.indexOf(columnName)
                // Concatenate the value.
                other.getTabular(otherRowIndex)(otherColumnIndex) :: acc
              }
              else {

                // Concatenate "" to the accumulator
                "" :: acc
              }
            }
          )

          newRow :: acc
        }
      )

      Some(new Table(newColumnNames, newTabular1 ++ newTabular2))
    }
  }
}

object Table {
  // 1.2
  def apply(s: String): Table = {

    // Split the string at the newline character.
    val lines = s.split("\n")

    // Get the line containing the column names.
    val columnNames: Line = lines(0).split(",").toList

    // Get the lines containing the data.
    // Remove first line containing the column names.
    val dataLines: List[String] = lines.slice(1, lines.length).toList

    // Parse the lines containing the data.
    // Take each line and make a new list with the split tokens.
    val tabular: List[List[String]] = dataLines.map(_.split(",", -1).toList)

    // Return a new table.
    new Table(columnNames, tabular)
  }
}
