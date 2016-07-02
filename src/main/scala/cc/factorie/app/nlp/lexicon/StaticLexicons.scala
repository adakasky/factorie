/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
package cc.factorie.app.nlp.lexicon

import java.net.URL
import java.nio.file.{Paths, Files, Path}

import cc.factorie.app.nlp.lexicon.{iesl => Iesl, uscensus => Uscensus, wikipedia => Wikipedia, ssdi => Ssdi, mandarin => Mandarin}
import cc.factorie.app.strings.StringSegmenter
import cc.factorie.app.nlp.lemma.{Lemmatizer,LowercaseLemmatizer}
import java.io.{InputStream, File}
import cc.factorie.util.{ModelProvider, ClasspathURL}

import scala.reflect.{ClassTag, classTag}
import scala.language.implicitConversions
import scala.util.Try

trait LexiconsProvider {
  def lexiconRoot:String
  implicit def provide[L : ClassTag]:ModelProvider[L]
}

object LexiconsProvider {
  import cc.factorie.util.ISAble._

  private def lexiconNamePieces[L:ClassTag]:Seq[String] = {
    val arr = classTag[L].runtimeClass.getName.split("""\.""").map(_.stripSuffix("$"))
    val fileName = arr.last.zipWithIndex.flatMap {
      case (u, 0) => u.toLower.toString
      case (u, _) if u.isUpper => "-" + u.toLower
      case (l, _) => l.toString
    }.mkString("") + ".txt"
    arr.init.map(_.toLowerCase) ++ Seq(fileName)
  }

  private def fullLexiconName[L:ClassTag] = lexiconNamePieces[L].mkString("/")
  private def shortLexiconName[L:ClassTag] = lexiconNamePieces[L].drop(5).mkString("/")


  def fromFile(f:File, useFullPath:Boolean = false):LexiconsProvider = new LexiconsProvider {
    lazy val lexiconRoot = f.getAbsolutePath
    override implicit def provide[L : ClassTag]: ModelProvider[L] = new ModelProvider[L] {
      private val path = f.toPath.resolve(if(useFullPath) fullLexiconName[L] else shortLexiconName[L])
      val coordinates = path.toString
      val provide:InputStream = buffered(path)
    }
  }

  def fromUrl(u:URL, useFullPath:Boolean = false):LexiconsProvider = new LexiconsProvider {
    lazy val lexiconRoot = u.toString
    implicit def provide[L:ClassTag]: ModelProvider[L] = new ModelProvider[L] {
      private val modelUrl = new URL(u, if(useFullPath) fullLexiconName[L] else shortLexiconName[L])
      val provide: InputStream = buffered(modelUrl)
      val coordinates: String = modelUrl.toString
    }
  }

  implicit def providePath(p:Path):LexiconsProvider = fromFile(p.toFile, false)
  implicit def provideFile(f:File):LexiconsProvider = fromFile(f,false)
  implicit def provideURL(u:URL):LexiconsProvider = fromUrl(u, false)

  def fromString(s:String, useFullPath:Boolean=false):LexiconsProvider = s match {
    case cp if cp.toLowerCase == "classpath" => classpath(useFullPath)
    case urlS if Try(new URL(urlS)).isSuccess => fromUrl(new URL(urlS), useFullPath)
    case p => fromFile(new File(p), useFullPath)
  }

  @deprecated("This exists to preserve legacy functionality", "10/27/15")
  def classpath(useFullPath:Boolean=true):LexiconsProvider = new LexiconsProvider {
    def lexiconRoot = "classpath"
    implicit def provide[L: ClassTag]: ModelProvider[L] = new ModelProvider[L] {
      private def url = if(useFullPath) ClasspathURL.fromDirectory[Lexicon](shortLexiconName[L]) else this.getClass.getResource("/" + shortLexiconName[L])
      def coordinates: String = url.toString
      def provide: InputStream = url
    }
  }

  /*
  @deprecated("This exists to preserve legacy functionality", "10/05/15")
  def classpath:LexiconsProvider = new LexiconsProvider {
    //lazy val lexiconRoot = ClasspathURL.fromDirectory[Lexicon]("")
    lazy val lexiconRoot = Lexicon.getClass.getResource("")
    implicit def provide[L : ClassTag]: ModelProvider[L] = new ModelProvider[L] {
      private val url = {
        println("root " + lexiconRoot)
        println("shortname" + shortLexiconName[L])
        new URL(lexiconRoot, shortLexiconName[L])
      }
      val coordinates: String = url.toString
      val provide: InputStream = buffered(url)
    }
  }
  */
}



trait ProvidedLexicon[L] {
  this: MutableLexicon =>

  def provider:ModelProvider[L]

  synchronized {
    this.++=(provider.provide)
  }
}

class ProvidedTriePhraseLexicon[L]()(implicit val provider:ModelProvider[L], ct:ClassTag[L]) extends TriePhraseLexicon(ct.runtimeClass.getName) with ProvidedLexicon[L]

class GenericLexicon(name:String, val provider:ModelProvider[GenericLexicon]) extends TriePhraseLexicon(name) with ProvidedLexicon[GenericLexicon]

class StaticLexicons()(implicit lp:LexiconsProvider) {

  import lp._

  object iesl {

    object Continents extends Iesl.Continents()(lp.provide[Iesl.Continents])
    object Country extends Iesl.Country()(lp.provide[Iesl.Country])
    object City extends Iesl.City()(lp.provide[Iesl.City])
    object UsState extends Iesl.UsState()(lp.provide[Iesl.UsState])
    object PlaceSuffix extends Iesl.PlaceSuffix()(lp.provide[Iesl.PlaceSuffix])
    object JobTitle extends Iesl.JobTitle()(lp.provide[Iesl.JobTitle])
    object Money extends Iesl.Money()(lp.provide[Iesl.Money])
    object Company extends Iesl.Company()(lp.provide[Iesl.Company])
    object OrgSuffix extends Iesl.OrgSuffix()(lp.provide[Iesl.OrgSuffix])
    object Month extends Iesl.Month()(lp.provide[Iesl.Month])
    object Day extends Iesl.Day()(lp.provide[Iesl.Day])
    object PersonHonorific extends Iesl.PersonHonorific()(lp.provide[Iesl.PersonHonorific])
    object PersonFirstHighest extends Iesl.PersonFirstHighest()(lp.provide[Iesl.PersonFirstHighest])
    object PersonFirstHigh extends Iesl.PersonFirstHigh()(lp.provide[Iesl.PersonFirstHigh])
    object PersonFirstMedium extends Iesl.PersonFirstMedium()(lp.provide[Iesl.PersonFirstMedium])
    object PersonLastHighest extends Iesl.PersonLastHighest()(lp.provide[Iesl.PersonLastHighest])
    object PersonLastHigh extends Iesl.PersonLastHigh()(lp.provide[Iesl.PersonLastHigh])
    object PersonLastMedium extends Iesl.PersonLastMedium()(lp.provide[Iesl.PersonLastMedium])
    object Say extends Iesl.Say()(lp.provide[Iesl.Say])
    object Demonym extends Iesl.Demonym()(lp.provide[Iesl.Demonym])
    object DemonymMap extends Iesl.DemonymMap()(lp.provide[Iesl.Demonym])

    object AllPlaces extends TrieUnionLexicon("places", Continents, Country, City, UsState)

    object PersonFirst extends TrieUnionLexicon("person-first", PersonFirstHighest, PersonFirstHigh, PersonFirstMedium)

    object PersonLast extends TrieUnionLexicon("person-last", PersonLastHighest, PersonLastHigh, PersonLastMedium)

  }

  object ssdi {
    object PersonFirstHighest extends Ssdi.PersonFirstHighest()(lp.provide[Ssdi.PersonFirstHighest])
    object PersonFirstHigh extends Ssdi.PersonFirstHigh()(lp.provide[Ssdi.PersonFirstHigh])
    object PersonFirstMedium extends Ssdi.PersonFirstMedium()(lp.provide[Ssdi.PersonFirstMedium])
    object PersonLastHighest extends Ssdi.PersonLastHighest()(lp.provide[Ssdi.PersonLastHighest])
    object PersonLastHigh extends Ssdi.PersonLastHigh()(lp.provide[Ssdi.PersonLastHigh])
    object PersonLastMedium extends Ssdi.PersonLastMedium()(lp.provide[Ssdi.PersonLastMedium])

    object PersonFirst extends TrieUnionLexicon("person-first", PersonFirstHighest, PersonFirstHigh, PersonFirstMedium)

    object PersonLast extends TrieUnionLexicon("person-last", PersonLastHighest, PersonLastHigh, PersonLastMedium)

  }

  object uscensus {

    object PersonFirstFemale extends Uscensus.PersonFirstFemale()(lp.provide[Uscensus.PersonFirstFemale])
    object PersonFirstMale extends Uscensus.PersonFirstMale()(lp.provide[Uscensus.PersonFirstMale])
    object PersonLast extends Uscensus.PersonLast()(lp.provide[Uscensus.PersonLast])

  }

  object wikipedia {
    object Battle extends Wikipedia.Battle()(lp.provide[Wikipedia.Battle])
    object BattleRedirect extends Wikipedia.BattleRedirect()(lp.provide[Wikipedia.BattleRedirect])
    object BattleAndRedirect extends TrieUnionLexicon("battle-and-redirect", Battle, BattleRedirect)
    object BattleDisambiguation extends Wikipedia.BattleDisambiguation()(lp.provide[Wikipedia.BattleDisambiguation])
    object Book extends Wikipedia.Book()(lp.provide[Wikipedia.Book])
    object BookRedirect extends Wikipedia.BookRedirect()(lp.provide[Wikipedia.BookRedirect])
    object BookAndRedirect extends TrieUnionLexicon("book-and-redirect", Book, BookRedirect)
    object BookDisambiguation extends Wikipedia.BookDisambiguation()(lp.provide[Wikipedia.BookDisambiguation])
    object Business extends Wikipedia.Business()(lp.provide[Wikipedia.Business])
    object BusinessRedirect extends Wikipedia.BusinessRedirect()(lp.provide[Wikipedia.BusinessRedirect])
    object BusinessAndRedirect extends TrieUnionLexicon("business-and-redirect", Business, BusinessRedirect)
    object BusinessDisambiguation extends Wikipedia.BusinessDisambiguation()(lp.provide[Wikipedia.BusinessDisambiguation])
    object Competition extends Wikipedia.Competition()(lp.provide[Wikipedia.Competition])
    object CompetitionRedirect extends Wikipedia.CompetitionRedirect()(lp.provide[Wikipedia.CompetitionRedirect])
    object CompetitionAndRedirect extends TrieUnionLexicon("competition-and-redirect", Competition, CompetitionRedirect)
    object CompetitionDisambiguation extends Wikipedia.CompetitionDisambiguation()(lp.provide[Wikipedia.CompetitionDisambiguation])
    object Event extends Wikipedia.Event()(lp.provide[Wikipedia.Event])
    object EventRedirect extends Wikipedia.EventRedirect()(lp.provide[Wikipedia.EventRedirect])
    object EventAndRedirect extends TrieUnionLexicon("event-and-redirect", Event, EventRedirect)
    object EventDisambiguation extends Wikipedia.EventDisambiguation()(lp.provide[Wikipedia.EventDisambiguation])
    object Film extends Wikipedia.Film()(lp.provide[Wikipedia.Film])
    object FilmRedirect extends Wikipedia.FilmRedirect()(lp.provide[Wikipedia.FilmRedirect])
    object FilmAndRedirect extends TrieUnionLexicon("film-and-redirect", Film, FilmRedirect)
    object FilmDisambiguation extends Wikipedia.FilmDisambiguation()(lp.provide[Wikipedia.FilmDisambiguation])
    object Location extends Wikipedia.Location()(lp.provide[Wikipedia.Location])
    object LocationRedirect extends Wikipedia.LocationRedirect()(lp.provide[Wikipedia.LocationRedirect])
    object LocationAndRedirect extends TrieUnionLexicon("location-and-redirect", Location, LocationRedirect)
    object LocationDisambiguation extends Wikipedia.LocationDisambiguation()(lp.provide[Wikipedia.LocationDisambiguation])
    object ManMadeThing extends Wikipedia.ManMadeThing()(lp.provide[Wikipedia.ManMadeThing])
    object ManMadeThingRedirect extends Wikipedia.ManMadeThingRedirect()(lp.provide[Wikipedia.ManMadeThingRedirect])
    object ManMadeThingAndRedirect extends TrieUnionLexicon("man-made-thing-and-redirect", ManMadeThing, ManMadeThingRedirect)
    object ManMadeThingDisambiguation extends Wikipedia.ManMadeThingDisambiguation()(lp.provide[Wikipedia.ManMadeThingDisambiguation])
    object Organization extends Wikipedia.Organization()(lp.provide[Wikipedia.Organization])
    object OrganizationRedirect extends Wikipedia.OrganizationRedirect()(lp.provide[Wikipedia.OrganizationRedirect])
    object OrganizationAndRedirect extends TrieUnionLexicon("organization-and-redirect", Organization, OrganizationRedirect)
    object OrganizationDisambiguation extends Wikipedia.OrganizationDisambiguation()(lp.provide[Wikipedia.OrganizationDisambiguation])
    object Person extends Wikipedia.Person()(lp.provide[Wikipedia.Person])
    object PersonRedirect extends Wikipedia.PersonRedirect()(lp.provide[Wikipedia.PersonRedirect])
    object PersonAndRedirect extends TrieUnionLexicon("person-and-redirect", Person, PersonRedirect)
    object PersonDisambiguation extends Wikipedia.PersonDisambiguation()(lp.provide[Wikipedia.PersonDisambiguation])
    object Song extends Wikipedia.Song()(lp.provide[Wikipedia.Song])
    object SongRedirect extends Wikipedia.SongRedirect()(lp.provide[Wikipedia.SongRedirect])
    object SongAndRedirect extends TrieUnionLexicon("song-and-redirect", Song, SongRedirect)
    object SongDisambiguation extends Wikipedia.SongDisambiguation()(lp.provide[Wikipedia.SongDisambiguation])

  }

  object mandarin {
    object SurnamePinyin extends Mandarin.SurnamePinyin()(lp.provide[Mandarin.SurnamePinyin])
    object GivenNamePinyin extends Mandarin.GivenNamePinyin()(lp.provide[Mandarin.GivenNamePinyin])
  }
  
  object spanish {
    
    object Continents extends Iesl.es.Continents()(lp.provide[Iesl.es.Continents])
    object Day extends Iesl.es.Day()(lp.provide[Iesl.es.Day])
    object Month extends Iesl.es.Month()(lp.provide[Iesl.es.Month])
    object PersonFirst extends Iesl.es.PersonFirst()(lp.provide[Iesl.es.PersonFirst])
    object PersonLast extends Iesl.es.PersonLast()(lp.provide[Iesl.es.PersonLast])
    object Location extends Iesl.es.Location()(lp.provide[Iesl.es.Location])
    object Miscellaneous extends Iesl.es.Miscellaneous()(lp.provide[Iesl.es.Miscellaneous])
    object Person extends Iesl.es.Person()(lp.provide[Iesl.es.Person])
    object Organization extends Iesl.es.Organization()(lp.provide[Iesl.es.Organization])
    object PersonHonorific extends Iesl.es.PersonHonorific()(lp.provide[Iesl.es.PersonHonorific])
    object OrgSuffix extends Iesl.es.OrgSuffix()(lp.provide[Iesl.es.OrgSuffix])
    object Demonym extends Iesl.es.Demonym()(lp.provide[Iesl.es.Demonym])
    
    object WikiBook extends Wikipedia.es.Book()(lp.provide[Wikipedia.es.Book])
    object WikiFilm extends Wikipedia.es.Film()(lp.provide[Wikipedia.es.Film])
    object WikiEvent extends Wikipedia.es.Event()(lp.provide[Wikipedia.es.Event])
    object WikiBusiness extends Wikipedia.es.Business()(lp.provide[Wikipedia.es.Business])
    object WikiLocation extends Wikipedia.es.Location()(lp.provide[Wikipedia.es.Location])
    object WikiLocationRedirect extends Wikipedia.es.LocationRedirect()(lp.provide[Wikipedia.es.LocationRedirect])
    object WikiLocationAndRedirect extends TrieUnionLexicon("es-location-and-redirect", WikiLocation, WikiLocationRedirect)
    object WikiPerson extends Wikipedia.es.Person()(lp.provide[Wikipedia.es.Person])
    object WikiPersonRedirect extends Wikipedia.es.PersonRedirect()(lp.provide[Wikipedia.es.PersonRedirect])
    object WikiPersonAndRedirect extends TrieUnionLexicon("es-person-and-redirect", WikiPerson, WikiPersonRedirect)
    object WikiOrganization extends Wikipedia.es.Organization()(lp.provide[Wikipedia.es.Organization])
    object WikiOrganizationRedirect extends Wikipedia.es.OrganizationRedirect()(lp.provide[Wikipedia.es.OrganizationRedirect])
    object WikiOrganizationAndRedirect extends TrieUnionLexicon("es-organization-and-redirect", WikiOrganization, WikiOrganizationRedirect)

    object MajorBaseFightCrimeTypeFree extends major.BaseFightCrimeTypeFree()(lp.provide[major.BaseFightCrimeTypeFree])
    object MajorBookWiki extends major.BookWiki()(lp.provide[major.BookWiki])
    object MajorBusinessJobTitleFree extends major.BusinessJobTitleFree()(lp.provide[major.BusinessJobTitleFree])
    object MajorBusinessWiki extends major.BusinessWiki()(lp.provide[major.BusinessWiki])
    object MajorContinentsIesl extends major.ContinentsIesl()(lp.provide[major.ContinentsIesl])
    object MajorDayIesl extends major.DayIesl()(lp.provide[major.DayIesl])
    object MajorDemonymIesl extends major.DemonymIesl()(lp.provide[major.DemonymIesl])
    object MajorEducationEducationalInstitutionFree extends major.EducationEducationalInstitutionFree()(lp.provide[major.EducationEducationalInstitutionFree])
    object MajorEventWiki extends major.EventWiki()(lp.provide[major.EventWiki])
    object MajorFilmWiki extends major.FilmWiki()(lp.provide[major.FilmWiki])
    object MajorFirstNameFemaleExtr extends major.FirstNameFemaleExtr()(lp.provide[major.FirstNameFemaleExtr])
    object MajorFirstNameMaleExtr extends major.FirstNameMaleExtr()(lp.provide[major.FirstNameMaleExtr])
    object MajorGovernmentPoliticalDistrictFree extends major.GovernmentPoliticalDistrictFree()(lp.provide[major.GovernmentPoliticalDistrictFree])
    object MajorGovernmentPoliticalIdeologyFree extends major.GovernmentPoliticalIdeologyFree()(lp.provide[major.GovernmentPoliticalIdeologyFree])
    object MajorGovernmentPoliticalPartyFree extends major.GovernmentPoliticalPartyFree()(lp.provide[major.GovernmentPoliticalPartyFree])
    object MajorLastNameExtr extends major.LastNameExtr()(lp.provide[major.LastNameExtr])
    object MajorLawLegalSubjectFree extends major.LawLegalSubjectFree()(lp.provide[major.LawLegalSubjectFree])
    object MajorLocationAdministrativeDivisionFree extends major.LocationAdministrativeDivisionFree()(lp.provide[major.LocationAdministrativeDivisionFree])
    object MajorLocationCityEnOsmp extends major.LocationCityEnOsmp()(lp.provide[major.LocationCityEnOsmp])
    object MajorLocationCityEnWiki extends major.LocationCityEnWiki()(lp.provide[major.LocationCityEnWiki])
    object MajorLocationCityEsOsmp extends major.LocationCityEsOsmp()(lp.provide[major.LocationCityEsOsmp])
    object MajorLocationCityEsWiki extends major.LocationCityEsWiki()(lp.provide[major.LocationCityEsWiki])
    object MajorLocationCitytownFree extends major.LocationCitytownFree()(lp.provide[major.LocationCitytownFree])
    object MajorLocationCountryEnWiki extends major.LocationCountryEnWiki()(lp.provide[major.LocationCountryEnWiki])
    object MajorLocationCountryEsWiki extends major.LocationCountryEsWiki()(lp.provide[major.LocationCountryEsWiki])
    object MajorLocationCountryFree extends major.LocationCountryFree()(lp.provide[major.LocationCountryFree])
    object MajorLocationIesl extends major.LocationIesl()(lp.provide[major.LocationIesl])
    object MajorLocationRedirectWiki extends major.LocationRedirectWiki()(lp.provide[major.LocationRedirectWiki])
    object MajorLocationStateEnWiki extends major.LocationStateEnWiki()(lp.provide[major.LocationStateEnWiki])
    object MajorLocationStateEsWiki extends major.LocationStateEsWiki()(lp.provide[major.LocationStateEsWiki])
    object MajorLocationUndefinedEnWiki extends major.LocationUndefinedEnWiki()(lp.provide[major.LocationUndefinedEnWiki])
    object MajorLocationUndefinedEsWiki extends major.LocationUndefinedEsWiki()(lp.provide[major.LocationUndefinedEsWiki])
    object MajorLocationWiki extends major.LocationWiki()(lp.provide[major.LocationWiki])
    object MajorMiscellaneousIesl extends major.MiscellaneousIesl()(lp.provide[major.MiscellaneousIesl])
    object MajorMonthIesl extends major.MonthIesl()(lp.provide[major.MonthIesl])
    object MajorOrgSuffixIesl extends major.OrgSuffixIesl()(lp.provide[major.OrgSuffixIesl])
    object MajorOrganizationIesl extends major.OrganizationIesl()(lp.provide[major.OrganizationIesl])
    object MajorOrganizationOrganizationFree extends major.OrganizationOrganizationFree()(lp.provide[major.OrganizationOrganizationFree])
    object MajorOrganizationOrganizationSectorFree extends major.OrganizationOrganizationSectorFree()(lp.provide[major.OrganizationOrganizationSectorFree])
    object MajorOrganizationRedirectWiki extends major.OrganizationRedirectWiki()(lp.provide[major.OrganizationRedirectWiki])
    object MajorOrganizationWiki extends major.OrganizationWiki()(lp.provide[major.OrganizationWiki])
    object MajorPeopleCauseOfDeathFree extends major.PeopleCauseOfDeathFree()(lp.provide[major.PeopleCauseOfDeathFree])
    object MajorPeoplePersonFree extends major.PeoplePersonFree()(lp.provide[major.PeoplePersonFree])
    object MajorPeopleProfessionFree extends major.PeopleProfessionFree()(lp.provide[major.PeopleProfessionFree])
    object MajorPersonFirstIesl extends major.PersonFirstIesl()(lp.provide[major.PersonFirstIesl])
    object MajorPersonHonorificIesl extends major.PersonHonorificIesl()(lp.provide[major.PersonHonorificIesl])
    object MajorPersonIesl extends major.PersonIesl()(lp.provide[major.PersonIesl])
    object MajorPersonLastIesl extends major.PersonLastIesl()(lp.provide[major.PersonLastIesl])
    object MajorPersonRedirectWiki extends major.PersonRedirectWiki()(lp.provide[major.PersonRedirectWiki])
    object MajorPersonWiki extends major.PersonWiki()(lp.provide[major.PersonWiki])
    object MajorReligionReligionFree extends major.ReligionReligionFree()(lp.provide[major.ReligionReligionFree])
    object MajorReligionReligiousOrganizationFree extends major.ReligionReligiousOrganizationFree()(lp.provide[major.ReligionReligiousOrganizationFree])

    object MinorBaseFightCrimeTypeFree extends minor.BaseFightCrimeTypeFree()(lp.provide[minor.BaseFightCrimeTypeFree])
    object MinorBusinessJobTitleFree extends minor.BusinessJobTitleFree()(lp.provide[minor.BusinessJobTitleFree])
    object MinorBusinessWiki extends minor.BusinessWiki()(lp.provide[minor.BusinessWiki])
    object MinorEducationEducationalInstitutionFree extends minor.EducationEducationalInstitutionFree()(lp.provide[minor.EducationEducationalInstitutionFree])
    object MinorFirstNameFemaleExtr extends minor.FirstNameFemaleExtr()(lp.provide[minor.FirstNameFemaleExtr])
    object MinorFirstNameMaleExtr extends minor.FirstNameMaleExtr()(lp.provide[minor.FirstNameMaleExtr])
    object MinorGovernmentPoliticalDistrictFree extends minor.GovernmentPoliticalDistrictFree()(lp.provide[minor.GovernmentPoliticalDistrictFree])
    object MinorGovernmentPoliticalIdeologyFree extends minor.GovernmentPoliticalIdeologyFree()(lp.provide[minor.GovernmentPoliticalIdeologyFree])
    object MinorGovernmentPoliticalPartyFree extends minor.GovernmentPoliticalPartyFree()(lp.provide[minor.GovernmentPoliticalPartyFree])
    object MinorLastNameExtr extends minor.LastNameExtr()(lp.provide[minor.LastNameExtr])
    object MinorLawLegalSubjectFree extends minor.LawLegalSubjectFree()(lp.provide[minor.LawLegalSubjectFree])
    object MinorLocationAdministrativeDivisionFree extends minor.LocationAdministrativeDivisionFree()(lp.provide[minor.LocationAdministrativeDivisionFree])
    object MinorLocationCityEnOsmp extends minor.LocationCityEnOsmp()(lp.provide[minor.LocationCityEnOsmp])
    object MinorLocationCityEnWiki extends minor.LocationCityEnWiki()(lp.provide[minor.LocationCityEnWiki])
    object MinorLocationCityEsOsmp extends minor.LocationCityEsOsmp()(lp.provide[minor.LocationCityEsOsmp])
    object MinorLocationCityEsWiki extends minor.LocationCityEsWiki()(lp.provide[minor.LocationCityEsWiki])
    object MinorLocationCitytownFree extends minor.LocationCitytownFree()(lp.provide[minor.LocationCitytownFree])
    object MinorLocationCountryEnWiki extends minor.LocationCountryEnWiki()(lp.provide[minor.LocationCountryEnWiki])
    object MinorLocationCountryEsWiki extends minor.LocationCountryEsWiki()(lp.provide[minor.LocationCountryEsWiki])
    object MinorLocationCountryFree extends minor.LocationCountryFree()(lp.provide[minor.LocationCountryFree])
    object MinorLocationIesl extends minor.LocationIesl()(lp.provide[minor.LocationIesl])
    object MinorLocationRedirectWiki extends minor.LocationRedirectWiki()(lp.provide[minor.LocationRedirectWiki])
    object MinorLocationStateEnWiki extends minor.LocationStateEnWiki()(lp.provide[minor.LocationStateEnWiki])
    object MinorLocationStateEsWiki extends minor.LocationStateEsWiki()(lp.provide[minor.LocationStateEsWiki])
    object MinorLocationUndefinedEnWiki extends minor.LocationUndefinedEnWiki()(lp.provide[minor.LocationUndefinedEnWiki])
    object MinorLocationUndefinedEsWiki extends minor.LocationUndefinedEsWiki()(lp.provide[minor.LocationUndefinedEsWiki])
    object MinorLocationWiki extends minor.LocationWiki()(lp.provide[minor.LocationWiki])
    object MinorOrganizationIesl extends minor.OrganizationIesl()(lp.provide[minor.OrganizationIesl])
    object MinorOrganizationOrganizationFree extends minor.OrganizationOrganizationFree()(lp.provide[minor.OrganizationOrganizationFree])
    object MinorOrganizationOrganizationSectorFree extends minor.OrganizationOrganizationSectorFree()(lp.provide[minor.OrganizationOrganizationSectorFree])
    object MinorOrganizationRedirectWiki extends minor.OrganizationRedirectWiki()(lp.provide[minor.OrganizationRedirectWiki])
    object MinorOrganizationWiki extends minor.OrganizationWiki()(lp.provide[minor.OrganizationWiki])
    object MinorPeopleCauseOfDeathFree extends minor.PeopleCauseOfDeathFree()(lp.provide[minor.PeopleCauseOfDeathFree])
    object MinorPeoplePersonFree extends minor.PeoplePersonFree()(lp.provide[minor.PeoplePersonFree])
    object MinorPeopleProfessionFree extends minor.PeopleProfessionFree()(lp.provide[minor.PeopleProfessionFree])
    object MinorPersonFirstIesl extends minor.PersonFirstIesl()(lp.provide[minor.PersonFirstIesl])
    object MinorPersonHonorificIesl extends minor.PersonHonorificIesl()(lp.provide[minor.PersonHonorificIesl])
    object MinorPersonIesl extends minor.PersonIesl()(lp.provide[minor.PersonIesl])
    object MinorPersonLastIesl extends minor.PersonLastIesl()(lp.provide[minor.PersonLastIesl])
    object MinorPersonRedirectWiki extends minor.PersonRedirectWiki()(lp.provide[minor.PersonRedirectWiki])
    object MinorPersonWiki extends minor.PersonWiki()(lp.provide[minor.PersonWiki])
    object MinorReligionReligionFree extends minor.ReligionReligionFree()(lp.provide[minor.ReligionReligionFree])
    object MinorReligionReligiousOrganizationFree extends minor.ReligionReligiousOrganizationFree()(lp.provide[minor.ReligionReligiousOrganizationFree])
  }  

}

