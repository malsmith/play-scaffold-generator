package generators.utils

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class ModelProvider(config : AppConfig) {

  private val jdbcDriver = config.jdbcDriver
  private val url = config.url
  private val user = config.user
  private val password = config.password
  private val excluded = Seq("play_evolutions")

	import scala.concurrent.ExecutionContext.Implicits.global

  val slickDriver = config.slickDriver
  lazy val db = slickDriver.backend.Database.forURL(url,driver=jdbcDriver, user = user, password = password)
  lazy val model = { 
    val xtables = slickDriver.defaultTables
    // MALCOLM SMITH 
    // x = for( tbl <- xtables if tbl.
    // val result = Await.result(xtables, Duration.Inf)
    // val tables = result.filterNot(t => !excluded.exists(_.equalsIgnoreCase(t.name.name)))
    val output = slickDriver.createModel(Some(xtables))
    Await.result(db.run(output), Duration.Inf)
  }

}


