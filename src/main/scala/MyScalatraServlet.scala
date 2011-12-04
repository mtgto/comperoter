import org.scalatra._
import java.net.URL
import scalate.ScalateSupport
import prpr._

class MyScalatraServlet extends ScalatraServlet with ScalateSupport {

  get("/") {
    contentType = "text/html"
    templateEngine.layout("WEB-INF/views/default.ssp")
  }

  notFound {
    // Try to render a ScalateTemplate if no route matched
    findTemplate(requestPath) map { path =>
      contentType = "text/html"
      layoutTemplate(path)
				 } orElse serveStaticResource() getOrElse resourceNotFound() 
  }
}
