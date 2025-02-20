package part2effects

import zio._

import java.util.concurrent.TimeUnit

object ZIODependencies extends ZIOAppDefault {

  case class User(name: String, email: String)

  class UserSubscrition(emailService: EmailService, userDataBase: UserDataBase) {
    def subscribeUser(user: User): Task[Unit] =
      for {
        _ <- emailService.email(user)
        _ <- userDataBase.insert(user)
      } yield()
  }

  object UserSubscrition {
    def create(emailService: EmailService, userDataBase: UserDataBase) =
      new UserSubscrition(emailService, userDataBase)

    val live: ZLayer[EmailService with UserDataBase, Nothing, UserSubscrition] =
      ZLayer.fromFunction(create _)
  }

  class EmailService {
    def email(user: User): Task[Unit] =
      ZIO.succeed(println(s"You've just been subscribed to Rock the JVM. Welcome, ${user.name}!"))
  }

  object EmailService {
    def create(): EmailService = new EmailService
    val live: ZLayer[Any, Nothing, EmailService] = ZLayer.succeed(create())
  }

  class UserDataBase(connectionPool: ConnectionPool) {
    def insert(user: User): Task[Unit] = for {
      conn <- connectionPool.get
      _ <- conn.runQuery(s"insert into subscribers(name, email) values (${user.name}, ${user.email})")
    } yield()
  }

  object UserDataBase {
    def create(connectionPool: ConnectionPool) =
      new UserDataBase(connectionPool)

    val live: ZLayer[ConnectionPool, Nothing, UserDataBase] = ZLayer.fromFunction(create _)
  }

  class ConnectionPool(nConnection: Int) {
    def get: Task[Connection] =
      ZIO.succeed(println("Acquired connection")) *> ZIO.succeed(Connection())
  }

  object ConnectionPool {
    def create(nConnections: Int) =
      new ConnectionPool(nConnections)

    def live(nConnections: Int): ZLayer[Any, Nothing, ConnectionPool] = ZLayer.succeed(create(nConnections))
  }

  case class Connection() {
    def runQuery(query: String): Task[Unit] =
      ZIO.succeed(println(s"Executing query: $query"))
  }

  val subscritionService = ZIO.succeed(  // Dependency injection
    UserSubscrition.create(
      EmailService.create(),
      UserDataBase.create(
        ConnectionPool.create(10)
      )
    )
  )

  /*
  * drawbacks
    - does not scale for many services
    - DI can be 100x worse
      - pass dependencies
      - not having all deps in the same place
      - passing dependencies multiple times
  * */

  def subscribe(user: User) = for {
    sub <- subscritionService //service is instantiated at the point of call
    _ <- sub.subscribeUser(user)
  } yield()

  // risk leaking resources if you subscribe multiple users in the same program

  val program = for {
    _ <- subscribe(User("Pedro", "pedro@mail.pt"))
    _ <- subscribe(User("Manel", "manel@mail.pt"))
  } yield ()

  // alternative
  def subscribe_v2(user: User): ZIO[UserSubscrition, Throwable, Unit] = for {
    sub <- ZIO.service[UserSubscrition] // ZIO[UserSubscription, Nothing, UserSubscription]
    _ <- sub.subscribeUser(user)
  } yield()

  val program_v2 = for {
    _ <- subscribe_v2(User("Pedro", "pedro@mail.pt"))
    _ <- subscribe_v2(User("Manel", "manel@mail.pt"))
  } yield ()

  /*
  * we don't need to care about dependencies until the end of the world
  * all ZIOs requiring this dependency will use the same instance
  * can use diferent instances of same type for different needs (e.g. testing)
  * layers can be created and composed much like regular ZIOs + rich API
  * */


  /*
  * ZLayers
  * */

  val connectionPoolLayer: ZLayer[Any, Nothing, ConnectionPool] = ZLayer.succeed(ConnectionPool.create(10))
  // a layer that requires a dependency (higher level) can be built with ZLayer.fromFunction,
  // and automatically fetch the function arguments and place them into the ZLayer's dependency / environment type argument

  val databaseLayer: ZLayer[ConnectionPool, Nothing, UserDataBase] = ZLayer.fromFunction(UserDataBase.create _)
  val emailServiceLayer: ZLayer[Any, Nothing, EmailService] = ZLayer.succeed(EmailService.create())
  val userSubscriptionServiceLayer: ZLayer[UserDataBase with EmailService, Nothing, UserSubscrition] = ZLayer.fromFunction(UserSubscrition.create _)

  // composing layers
  // vertical composition >>>
  val databaseLayerFull: ZLayer[Any, Nothing, UserDataBase] = connectionPoolLayer >>> databaseLayer

  // horizontal composition: combines the dependencies of both layers AND the values of both layers
  val subscriptionRequirementsLayer: ZLayer[Any, Nothing, UserDataBase with EmailService] = databaseLayerFull ++ emailServiceLayer

  //mix & match
  val userSubscritionLayer: ZLayer[Any, Nothing, UserSubscrition] =
    (subscriptionRequirementsLayer >>> userSubscriptionServiceLayer)//: ZLayer[Any, E, UserSubscrition]

  val runnableProgram = program_v2.provide(userSubscritionLayer)

  // magic
  val runnableProgram_v2 = program_v2.provide(
    UserSubscrition.live,
    EmailService.live,
    UserDataBase.live,
    ConnectionPool.live(10),
    // ZIO will tell you if you're missing a layer
    // and if you have multiple layers of the same type
    // and tell you the dependency graph
    // ZLayer.Debug.tree
    //ZLayer.Debug.mermaid
  )

  // magic v2
  val userSubscritionLayer_v2: ZLayer[Any, Nothing, UserSubscrition] = ZLayer.make[UserSubscrition](
    UserSubscrition.live,
    EmailService.live,
    UserDataBase.live,
    ConnectionPool.live(10),
  )

  // passthrough
  val dbWithPoolLayer: ZLayer[ConnectionPool, Nothing, ConnectionPool with UserDataBase] = UserDataBase.live.passthrough

  // service = take a dep and expose it as a value to further layers
  val dbService = ZLayer.service[UserDataBase]

  //  launch = craetes a ZIO that uses the services and never finishes
  val subscritionLaunch: ZIO[EmailService with UserDataBase, Nothing, Nothing] = UserSubscrition.live.launch

  // memoization

  /*
  * Already provided services: Clock, Random, System, Console
  * */

  val getTime = Clock.currentTime(TimeUnit.SECONDS)
  val randomvalue = Random.nextInt
  val sysvariable = System.env("HADOOP_HOME")
  val printlnEffefct = Console.printLine("This is ZIO")

  def run = runnableProgram_v2
  /*
  def run = program_v2.provide(
    ZLayer.succeed(
      UserSubscrition.create(
        EmailService.create(),
        UserDataBase.create(
          ConnectionPool.create(10)
        )
      )
    )
  )
*/
}


