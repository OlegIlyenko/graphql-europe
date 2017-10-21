package controllers.ed2017

import javax.inject.Inject

import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import play.api.Configuration
import play.api.mvc._
import repo.ContentRepo
import views.{Config, Edition}

class Schedule @Inject()(conf: Configuration, repo: ContentRepo) extends InjectedController {
  val edition = Edition.Berlin2017
  val config = conf.underlying.as[Config]("graphqlEurope")
  val conference = repo.conferencesByEdition(edition)

  def index = Action(implicit req ⇒ Ok(views.html.ed2017.schedule.schedule(withPath(config), conference)))
  
  def speakers = Action(implicit req ⇒ Ok(views.html.ed2017.schedule.speakers(withPath(config), conference)))

  def speaker(slug: String) = Action(implicit req ⇒
    conference.speakerBySlug(slug).fold(NotFound(views.html.ed2017.notFound(withPath(config), conference)))(s ⇒ Ok(views.html.ed2017.schedule.speaker(withPath(config), s, conference))))

  def talk(slug: String) = Action(implicit req ⇒
    conference.talkBySlug(slug).fold(NotFound(views.html.ed2017.notFound(withPath(config), conference)))(t ⇒ Ok(views.html.ed2017.schedule.talk(withPath(config), t, conference))))

  def withPath(config: Config)(implicit req: Request[_]) = config.copy(path = req.path)
}
