package repo

import javax.inject.{Inject, Singleton}

import play.api.Configuration
import views.Edition

@Singleton
class ContentRepo @Inject() (config: Configuration, ed2017: Conference2017) {
  val conferences = List(ed2017.conference)
  val conferencesByEdition = conferences.groupBy(_.edition).mapValues(_.head)

  // TODO: keep current
  val currentConference = conferencesByEdition(Edition.Berlin2017)
}
