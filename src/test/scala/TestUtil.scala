import java.util.concurrent.LinkedBlockingQueue

object TestUtil {
  def spawn(p: => Unit): Unit = {
    val t = new Thread() { override def run(): Unit = p }
    t.start()
  }

  // launch a list of threads in parallel, and wait till they all finish
  def spawnAndJoin(bodies: Seq[() => Unit]): Unit = {
    val qs = for {
      body <- bodies
      done = new LinkedBlockingQueue[Unit](1)
      _ = spawn { body(); done put () }
    } yield done
    qs foreach (_.take())
  }
}
