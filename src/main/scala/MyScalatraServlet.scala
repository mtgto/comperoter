import org.scalatra._
import java.net.URL
import scalate.ScalateSupport
import prpr._
import dispatch.json._
import sjson.json._
import scala.reflect.BeanInfo

class MyScalatraServlet extends ScalatraServlet with ScalateSupport {

  get("/") {
    contentType = "text/html"
    templateEngine.layout("WEB-INF/views/default.ssp")
  }

  post("/compile") {
    contentType ="text/json"
    val code = params("program")
    val target = params("target")
    import sjson.json.Serializer.SJSON
    @BeanInfo case class Response(status: String, detail: Map[String, String])
    val parser = new MyParser
    val result =
      parser.parseAll(parser.program, code) match {
	case parser.Success(program, _) => {
	  val compiler = new MyPrprCompiler(target)
	  Response("ok", Map("program"->compiler.convert(program)))
	}
	case parser.NoSuccess(message, _) => Response("fail", Map("description" -> message))
      }
    SJSON.out(result)
  }

  notFound {
    // Try to render a ScalateTemplate if no route matched
    findTemplate(requestPath) map { path =>
      contentType = "text/html"
      layoutTemplate(path)
				 } orElse serveStaticResource() getOrElse resourceNotFound() 
  }
}
