package graphql

import repo.ContentRepo
import sangria.schema._
import customScalars._
import sangria.marshalling.ScalaInput
import views._

object schema {
  case class Ctx(repo: ContentRepo, currentConference: Option[Conference] = None) {
    lazy val conf = currentConference getOrElse (throw new IllegalStateException("Conference is not defined"))
  }

  // TODO: keep current
  val EditionArg = Argument("edition", OptionInputType(Edition.graphqlType), ScalaInput.scalaInput(Edition.Berlin2017))

  val QueryType = ObjectType("Query", fields[Ctx, Unit](
    Field("conferences", ListType(Conference.graphqlType),
      resolve = c ⇒ c.ctx.repo.conferences.sortBy(_.year).reverse),
    Field("conference", OptionType(Conference.graphqlType),
      arguments = EditionArg :: Nil,
      resolve = c ⇒ c.ctx.repo.conferencesByEdition.get(c.arg(EditionArg)))
  ))

  val ConferenceSchema = Schema(QueryType, additionalTypes = List(
    Break.graphqlType,
    Registration.graphqlType,
    Lunch.graphqlType))
}
