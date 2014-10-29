import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror
import org.fusesource.scalate._
import java.io.{File, PrintWriter}

object Util {
  val normalizeRegex = """([a-z\d])([A-Z])""".r
  // キャメルケースをスネークケースに変換する
  def normalizeName(name: String) = {
    normalizeRegex.replaceAllIn(name, "$1_$2").toLowerCase
  }
}

// カラムの定義
case class ColumnDef(name: String, tpe: String, attributes: List[String]) {
  def nameNormalized = {
    Util.normalizeName(name)
  }

  def bindings = {
    Map(
      "name" -> name,
      "nameNormalized" -> nameNormalized,
      "type" -> tpe,
      "attributes" -> attributes
    )
  }
}

// モデルの定義
case class ModelDef(name: String, columns: List[ColumnDef]) {
  def nameNormalized = {
    Util.normalizeName(name)
  }

  def columnNameList = {
    columns.map(_.name).mkString(", ")
  }

  def bindings = {
    Map(
      "name" -> name,
      "nameNormalized" -> nameNormalized,
      "columns" -> columns.map(_.bindings),
      "columnNameList" -> columnNameList
    )
  }
}

object ModuleDef {
  def fromClassName(fullPath: String): ModelDef = {
    val classSymbol = currentMirror.staticClass(fullPath)
    val classType = classSymbol.typeSignature.erasure // Resolve poly type

    val name = classSymbol.name.decoded
    val cols = classType.members
      .filter(m => m.isPublic && m.isTerm && m.asTerm.isCaseAccessor)
      .map(m => {
        val name = m.name.decoded
        val tpe = m.typeSignature.toString.replace("=> ", "")
        val attrs = List()

        ColumnDef(name, tpe, attrs)
      })
      .toList
      .reverse

    ModelDef(name, cols)
  }
}

object Main {
  def main(args: Array[String]) {
    val currentPath = new File(".").getAbsoluteFile().getParent()
    val fullClassName = args(0)
    val templatePath = args(1)
    val outputPath = args(2)

    println(s"Current directory is: $currentPath")
    generateTableDef(fullClassName, templatePath, outputPath)
  }

  def generateTableDef(fullClassName: String, templatePath: String, outputPath: String) = {
    val engine = new TemplateEngine
    val modelDef = ModuleDef.fromClassName(fullClassName)
    val output = engine.layout(templatePath, modelDef.bindings)

    val outputDirectory = new File(outputPath)
    val outputFile = new File(outputDirectory, modelDef.name + "Generated.scala")
    val writer = new PrintWriter(outputFile)

    try {
      writer.write(output)
    } finally {
      writer.close()
    }
  }
}
