package views

import java.time._

import graphql.customScalars._
import graphql.schema.Ctx
import sangria.macros.derive._
import sangria.schema._

trait WithSlug {
  def slug: String
}

case class Speaker(
  name: String,
  photoUrl: Option[String],
  company: Option[String],
  twitter: Option[String],
  github: Option[String],
  description: Option[String],
  stub: Boolean = false
) extends WithSlug {
  lazy val slug = name.replaceAll("\\s+", "-").toLowerCase

  lazy val metaInfo = MetaInfo("Speaker: " + name, twitter = twitter)
}

object Speaker {
  private val TalkFormatArg = Argument("format", OptionInputType(TalkFormat.graphqlType))

  implicit lazy val graphqlType = deriveObjectType[Ctx, Speaker](
    ExcludeFields("stub"),
    AddFields(
      Field("url", StringType, resolve = c ⇒ c.ctx.conf.confUrl(s"/speakers/${c.value.slug}")),
      Field("talks", ListType(Talk.graphqlType),
      arguments = TalkFormatArg :: Nil,
      resolve = c ⇒ {
        val talks = c.ctx.conf.talksBySpeaker(c.value)

        c.withArgs(TalkFormatArg)(_.fold(talks)(f ⇒ talks.filter(_.format == f)))
      })))
}

object TalkFormat extends Enumeration {
  val Special, PanelDiscussion, Standard, Lightning = Value

  implicit val graphqlType: EnumType[TalkFormat.Value] = deriveEnumType[TalkFormat.Value]()
}

object ScheduleEntryType extends Enumeration {
  val Registration, Talk, Break, Lunch = Value

  implicit val graphqlType: EnumType[ScheduleEntryType.Value] = deriveEnumType[ScheduleEntryType.Value]()
}

abstract class ScheduleEntry(
  val entryType: ScheduleEntryType.Value
) {
  def startTime: LocalTime
  def endTime: LocalTime
  def duration: Duration
}

object ScheduleEntry {
  implicit val graphqlType = InterfaceType("ScheduleEntry", fields[Ctx, ScheduleEntry](
    Field("startTime", TimeType, resolve = _.value.startTime),
    Field("endTime", TimeType, resolve = _.value.endTime),
    Field("duration", DurationType, resolve = _.value.duration),
    Field("entryType", ScheduleEntryType.graphqlType, resolve = _.value.entryType)))
}

case class Break(startTime: LocalTime, endTime: LocalTime, duration: Duration) extends ScheduleEntry(ScheduleEntryType.Break)

object Break {
  implicit val graphqlType = deriveObjectType[Ctx, Break](Interfaces(ScheduleEntry.graphqlType))
}

case class Registration(startTime: LocalTime, endTime: LocalTime, duration: Duration) extends ScheduleEntry(ScheduleEntryType.Registration)

object Registration {
  implicit val graphqlType = deriveObjectType[Ctx, Registration](Interfaces(ScheduleEntry.graphqlType))
}

case class Lunch(startTime: LocalTime, endTime: LocalTime, duration: Duration) extends ScheduleEntry(ScheduleEntryType.Lunch)

object Lunch {
  implicit val graphqlType = deriveObjectType[Ctx, Lunch](Interfaces(ScheduleEntry.graphqlType))
}

case class Talk(
  title: String,
  description: String,
  cardUrl: String,
  format: TalkFormat.Value,
  startTime: LocalTime,
  endTime: LocalTime,
  duration: Duration,
  speakers: List[Speaker],
  slidesUrl: Option[String] = None,
  shortDescription: Option[String] = None,
  videoUrl: Option[String] = None
) extends ScheduleEntry(ScheduleEntryType.Talk) with WithSlug {
  lazy val slug = title.replaceAll("\\s+", "-").replaceAll("[^a-zA-Z0-9\\-]", "").toLowerCase

  lazy val youtubeId = videoUrl.flatMap { url ⇒
    val mat = Talk.YoutubePattern.matcher(url)

    if (mat.matches()) Option(mat.group(1))
    else None
  }

  lazy val metaInfo = {
    val speaker = speakers.headOption
    val by =
      if (speakers.nonEmpty)
        " (by " + speakers.map(_.name).sorted.mkString(", ") + ")"
      else
        ""

    MetaInfo("Talk: " + title + by, Some(cardUrl), if (speakers .isEmpty || speakers.size > 1) Some("graphqleu") else speaker.flatMap(_.twitter))
  }
}

object Talk {
  val YoutubePattern = """.*www\.youtube\.com\/watch\?v=([^&]+)&.*""".r.pattern

  implicit lazy val graphqlType: ObjectType[Ctx, Talk] =
    deriveObjectType[Ctx, Talk](
      ExcludeFields("shortDescription"),
      Interfaces(ScheduleEntry.graphqlType),
      AddFields(Field("url", StringType, resolve = c ⇒ c.ctx.conf.confUrl(s"/schedule/${c.value.slug}"))))
}

object SponsorType extends Enumeration {
  val Organiser, Partner, Opportunity, Platinum, Gold, Silver, Bronze = Value

  implicit val graphqlType: EnumType[SponsorType.Value] = deriveEnumType[SponsorType.Value]()
}

case class Sponsor(
  name: String,
  sponsorType: SponsorType.Value,
  url: String,
  logoUrl: String,
  description: Option[String],
  twitter: Option[String],
  github: Option[String])

object Sponsor {
  implicit val graphqlType = deriveObjectType[Ctx, Sponsor]()
}

object Edition extends Enumeration {
  val Berlin2017 = Value
  val Berlin2018 = Value

  implicit val graphqlType: EnumType[Edition.Value] = deriveEnumType[Edition.Value]()
}

case class Ticket(
  name: String,
  price: String,
  availableUntil: LocalDate,
  availableUntilText: String,
  url: String,
  soldOut: Boolean,
  available: Boolean)

object Ticket {
  implicit val graphqlType = deriveObjectType[Ctx, Ticket](
    ExcludeFields("availableUntilText"))
}

case class Address(
  country: String,
  city: String,
  zipCode: String,
  streetName: String,
  houseNumber: String,
  latitude: Double,
  longitude: Double)

object Address {
  implicit val graphqlType = deriveObjectType[Ctx, Address]()
}

object DirectionType extends Enumeration {
  val Airport, TrainStation, Car = Value

  implicit val graphqlType: EnumType[DirectionType.Value] = deriveEnumType[DirectionType.Value]()
}

case class Direction(`type`: DirectionType.Value, from: String, description: String)

object Direction {
  implicit val graphqlType = deriveObjectType[Ctx, Direction]()
}

case class Venue(
  name: String,
  url: String,
  phone: String,
  directions: List[Direction],
  address: Address)

object Venue {
  val DirectionTypeArg = Argument("type", OptionInputType(DirectionType.graphqlType))

  implicit val graphqlType = deriveObjectType[Ctx, Venue](
    ReplaceField("directions", Field("directions", ListType(Direction.graphqlType),
      arguments = DirectionTypeArg :: Nil,
      resolve = c ⇒ c.arg(DirectionTypeArg).fold(c.value.directions)(d ⇒ c.value.directions.filter(_.`type` == d))))
  )
}

case class Conference(
  name: String,
  edition: Edition.Value,
  description: String,
  shortDescription: String,
  location: String,
  tagLine: String,
  year: Int,
  venue: Option[Venue],
  dateStart: Option[LocalDate],
  dateEnd: Option[LocalDate],
  speakers: List[Speaker],
  sponsors: List[Sponsor],
  schedule: List[ScheduleEntry],
  team: List[TeamMember],
  tickets: List[Ticket],
  url: String,
  sponsorEmail: String,
  supportEmail: String,
  ticketsUrl: String,
  scheduleUrl: String,
  speakersUrl: String,
  sponsorsUrl: String,
  teamUrl: String,
  locationUrl: String,
  videosUrl: String
) {
  lazy val talks = schedule.collect {case t: Talk ⇒ t}

  def talksBySpeaker(speaker: Speaker) = talks.filter(_.speakers.contains(speaker))
  def talkBySlug(slug: String) = talks.find(_.slug == slug)
  def speakerBySlug(slug: String) = speakers.find(_.slug == slug)
  def confUrl(path: String) = url + path
  def normalUrl(path: String) = year + "/" + path
  def assetUrl(path: String) = year + "/" + path
}

object Conference {
  private val SectionArg = Argument("section", OptionInputType(TeamSection.graphqlType))
  private val EntryTypeArg = Argument("type", OptionInputType(ScheduleEntryType.graphqlType))
  private val TalkFormatArg = Argument("format", OptionInputType(TalkFormat.graphqlType))
  private val SponsorTypeArg = Argument("type", OptionInputType(SponsorType.graphqlType))

  implicit val graphqlType = deriveObjectType[Ctx, Conference](
    ReplaceField("speakers", Field("speakers", ListType(Speaker.graphqlType),
      resolve = c ⇒ UpdateCtx(c.value.speakers.filterNot(_.stub))(_ ⇒ c.ctx.copy(currentConference = Some(c.value))))),
    ReplaceField("team", Field("team", ListType(TeamMember.graphqlType),
      arguments = SectionArg :: Nil,
      resolve = c ⇒ UpdateCtx(c.withArgs(SectionArg)(_.fold(c.value.team)(s ⇒ c.value.team.filter(_.teamSection == s))))(_ ⇒ c.ctx.copy(currentConference = Some(c.value))))),
    ReplaceField("schedule", Field("schedule", ListType(ScheduleEntry.graphqlType),
      arguments = EntryTypeArg :: Nil,
      resolve = c ⇒ UpdateCtx(c.withArgs(EntryTypeArg)(_.fold(c.value.schedule)(et ⇒ c.value.schedule.filter(_.entryType == et))))(_ ⇒ c.ctx.copy(currentConference = Some(c.value))))),
    ReplaceField("sponsors", Field("sponsors", ListType(Sponsor.graphqlType),
      arguments = SponsorTypeArg :: Nil,
      resolve = c ⇒ {
        val sponsors = c.value.sponsors.sortBy(_.name)

        UpdateCtx(c.withArgs(SponsorTypeArg)(_.fold(sponsors)(s ⇒ sponsors.filter(_.sponsorType == s))))(_ ⇒ c.ctx.copy(currentConference = Some(c.value)))
      })),
    AddFields(Field("talks", ListType(Talk.graphqlType),
      arguments = TalkFormatArg :: Nil,
      resolve = c ⇒ {
        val talks = c.value.schedule.collect{case t: Talk ⇒ t}

        UpdateCtx(c.withArgs(TalkFormatArg)(_.fold(talks)(f ⇒ talks.filter(_.format == f))))(_ ⇒ c.ctx.copy(currentConference = Some(c.value)))
      }))
  )
}

object TeamSection extends Enumeration {
  val Core, SpecialThanks = Value

  implicit val graphqlType: EnumType[TeamSection.Value] = deriveEnumType[TeamSection.Value]()
}

case class TeamMember(
  name: String,
  photoUrl: Option[String],
  teamSection: TeamSection.Value,
  description: Option[String],
  twitter: Option[String],
  github: Option[String])

object TeamMember {
  implicit val graphqlType = deriveObjectType[Ctx, TeamMember]()
}

case class MetaInfo(description: String, bannerUrl: Option[String] = None, twitter: Option[String] = None)