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
    val ytables = xtables.map(tbl => tbl.filter(
    		meta => !(excluded.exists (_.equalsIgnoreCase(meta.name.name))
    		)))

//DEBUG    		
//    val ztables = xtables.map(tbl => tbl.filter(
//    		meta => (meta.name.name != "play_evolutions")
//    		&& !(excluded.exists (_.equalsIgnoreCase(meta.name.name))
//    		)))
//    
//     val lresult = Await.result(db.run(ztables),Duration.Inf)
//     lresult.foreach(r => println("name field=%s name=%s type=%s".format(r.name.name, r.name,r.typeName)))
    val output = slickDriver.createModel(Some(ytables))
    Await.result(db.run(output), Duration.Inf)
  }

}


