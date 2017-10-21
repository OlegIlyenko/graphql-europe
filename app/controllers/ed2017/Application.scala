package controllers.ed2017

import javax.inject.Inject

import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import play.api.Configuration
import play.api.mvc._
import repo.ContentRepo
import views.{Config, Edition}

class Application @Inject() (conf: Configuration, repo: ContentRepo) extends InjectedController {
  val edition = Edition.Berlin2017
  val config = conf.underlying.as[Config]("graphqlEurope")
  val conference = repo.conferencesByEdition(edition)

  val indexConference = conference.copy(
    speakers = conference.speakers.sortBy(_.slug).filterNot(_.stub))

  def index = Action(implicit req ⇒ Ok(views.html.ed2017.index(withPath(config), indexConference, req.queryString.contains("dark"), req.queryString.contains("topLogo"))))
  def videos = Action(implicit req ⇒ Ok(views.html.ed2017.videos(withPath(config), conference)))
  def codeOfConduct = Action(implicit req ⇒ Ok(views.html.ed2017.codeOfConduct(withPath(config), conference)))
  def imprint = Action(implicit req ⇒ Ok(views.html.ed2017.imprint(withPath(config), conference)))
  def team = Action(implicit req ⇒ Ok(views.html.ed2017.team(withPath(config), conference)))
  def sponsors = Action(implicit req ⇒ Ok(views.html.ed2017.sponsors(withPath(config), conference)))
  def location = Action(implicit req ⇒ Ok(views.html.ed2017.location(withPath(config), conference,
    conference.venue.getOrElse(throw new IllegalStateException("Venue is not defined")))))

  def withPath(config: Config)(implicit req: Request[_]) = config.copy(path = req.path)
}
