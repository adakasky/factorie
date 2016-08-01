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

import cc.factorie.app.nlp.lexicon.{iesl => Iesl, uscensus => Uscensus, wikipedia => Wikipedia, ssdi => Ssdi, mandarin => Mandarin, en => En, es => Es}
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
    object CountryAbbr2 extends Iesl.CountryAbbr2()(lp.provide[Iesl.CountryAbbr2])
    object CountryAbbr3 extends Iesl.CountryAbbr3()(lp.provide[Iesl.CountryAbbr3])

    object AllPlaces extends TrieUnionLexicon("places", Continents, Country, City, UsState, CountryAbbr2, CountryAbbr3)

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

  object english {
    object MajorAnimalParenWiki extends En.major.AnimalParenWiki()(lp.provide[En.major.AnimalParenWiki])
    object MajorAnimalRedirectParenWiki extends En.major.AnimalRedirectParenWiki()(lp.provide[En.major.AnimalRedirectParenWiki])
    object MajorAnimalRedirectWiki extends En.major.AnimalRedirectWiki()(lp.provide[En.major.AnimalRedirectWiki])
    object MajorAnimalWiki extends En.major.AnimalWiki()(lp.provide[En.major.AnimalWiki])
    object MajorAnimalsParenWiki extends En.major.AnimalsParenWiki()(lp.provide[En.major.AnimalsParenWiki])
    object MajorAnimalsRedirectParenWiki extends En.major.AnimalsRedirectParenWiki()(lp.provide[En.major.AnimalsRedirectParenWiki])
    object MajorAnimalsRedirectWiki extends En.major.AnimalsRedirectWiki()(lp.provide[En.major.AnimalsRedirectWiki])
    object MajorAnimalsWiki extends En.major.AnimalsWiki()(lp.provide[En.major.AnimalsWiki])
    object MajorBaseFightCrimeTypeFree extends En.major.BaseFightCrimeTypeFree()(lp.provide[En.major.BaseFightCrimeTypeFree])
    object MajorBattleDisambiguationWiki extends En.major.BattleDisambiguationWiki()(lp.provide[En.major.BattleDisambiguationWiki])
    object MajorBattleParenWiki extends En.major.BattleParenWiki()(lp.provide[En.major.BattleParenWiki])
    object MajorBattleRedirectParenWiki extends En.major.BattleRedirectParenWiki()(lp.provide[En.major.BattleRedirectParenWiki])
    object MajorBattleRedirectWiki extends En.major.BattleRedirectWiki()(lp.provide[En.major.BattleRedirectWiki])
    object MajorBattleWiki extends En.major.BattleWiki()(lp.provide[En.major.BattleWiki])
    object MajorBattlesDisambiguationWiki extends En.major.BattlesDisambiguationWiki()(lp.provide[En.major.BattlesDisambiguationWiki])
    object MajorBattlesParenWiki extends En.major.BattlesParenWiki()(lp.provide[En.major.BattlesParenWiki])
    object MajorBattlesRedirectParenWiki extends En.major.BattlesRedirectParenWiki()(lp.provide[En.major.BattlesRedirectParenWiki])
    object MajorBattlesRedirectWiki extends En.major.BattlesRedirectWiki()(lp.provide[En.major.BattlesRedirectWiki])
    object MajorBattlesWiki extends En.major.BattlesWiki()(lp.provide[En.major.BattlesWiki])
    object MajorBookDisambiguationWiki extends En.major.BookDisambiguationWiki()(lp.provide[En.major.BookDisambiguationWiki])
    object MajorBookParenWiki extends En.major.BookParenWiki()(lp.provide[En.major.BookParenWiki])
    object MajorBookRedirectParenWiki extends En.major.BookRedirectParenWiki()(lp.provide[En.major.BookRedirectParenWiki])
    object MajorBookRedirectWiki extends En.major.BookRedirectWiki()(lp.provide[En.major.BookRedirectWiki])
    object MajorBookWiki extends En.major.BookWiki()(lp.provide[En.major.BookWiki])
    object MajorBusinessDisambiguationWiki extends En.major.BusinessDisambiguationWiki()(lp.provide[En.major.BusinessDisambiguationWiki])
    object MajorBusinessJobTitleFree extends En.major.BusinessJobTitleFree()(lp.provide[En.major.BusinessJobTitleFree])
    object MajorBusinessParenWiki extends En.major.BusinessParenWiki()(lp.provide[En.major.BusinessParenWiki])
    object MajorBusinessRedirectParenWiki extends En.major.BusinessRedirectParenWiki()(lp.provide[En.major.BusinessRedirectParenWiki])
    object MajorBusinessRedirectWiki extends En.major.BusinessRedirectWiki()(lp.provide[En.major.BusinessRedirectWiki])
    object MajorBusinessWiki extends En.major.BusinessWiki()(lp.provide[En.major.BusinessWiki])
    object MajorCityIesl extends En.major.CityIesl()(lp.provide[En.major.CityIesl])
    object MajorCityParenWiki extends En.major.CityParenWiki()(lp.provide[En.major.CityParenWiki])
    object MajorCityRedirectParenWiki extends En.major.CityRedirectParenWiki()(lp.provide[En.major.CityRedirectParenWiki])
    object MajorCityRedirectWiki extends En.major.CityRedirectWiki()(lp.provide[En.major.CityRedirectWiki])
    object MajorCityWiki extends En.major.CityWiki()(lp.provide[En.major.CityWiki])
    object MajorCompanyIesl extends En.major.CompanyIesl()(lp.provide[En.major.CompanyIesl])
    object MajorCompetitionDisambiguationWiki extends En.major.CompetitionDisambiguationWiki()(lp.provide[En.major.CompetitionDisambiguationWiki])
    object MajorCompetitionParenWiki extends En.major.CompetitionParenWiki()(lp.provide[En.major.CompetitionParenWiki])
    object MajorCompetitionRedirectParenWiki extends En.major.CompetitionRedirectParenWiki()(lp.provide[En.major.CompetitionRedirectParenWiki])
    object MajorCompetitionRedirectWiki extends En.major.CompetitionRedirectWiki()(lp.provide[En.major.CompetitionRedirectWiki])
    object MajorCompetitionWiki extends En.major.CompetitionWiki()(lp.provide[En.major.CompetitionWiki])
    object MajorContinentsIesl extends En.major.ContinentsIesl()(lp.provide[En.major.ContinentsIesl])
    object MajorCountryIesl extends En.major.CountryIesl()(lp.provide[En.major.CountryIesl])
    object MajorCountryParenWiki extends En.major.CountryParenWiki()(lp.provide[En.major.CountryParenWiki])
    object MajorCountryRedirectParenWiki extends En.major.CountryRedirectParenWiki()(lp.provide[En.major.CountryRedirectParenWiki])
    object MajorCountryRedirectWiki extends En.major.CountryRedirectWiki()(lp.provide[En.major.CountryRedirectWiki])
    object MajorCountryWiki extends En.major.CountryWiki()(lp.provide[En.major.CountryWiki])
    object MajorDayIesl extends En.major.DayIesl()(lp.provide[En.major.DayIesl])
    object MajorDemonymIesl extends En.major.DemonymIesl()(lp.provide[En.major.DemonymIesl])
    object MajorDemonymsIesl extends En.major.DemonymsIesl()(lp.provide[En.major.DemonymsIesl])
    object MajorEducationEducationalInstitutionFree extends En.major.EducationEducationalInstitutionFree()(lp.provide[En.major.EducationEducationalInstitutionFree])
    object MajorEventDisambiguationWiki extends En.major.EventDisambiguationWiki()(lp.provide[En.major.EventDisambiguationWiki])
    object MajorEventParenWiki extends En.major.EventParenWiki()(lp.provide[En.major.EventParenWiki])
    object MajorEventRedirectParenWiki extends En.major.EventRedirectParenWiki()(lp.provide[En.major.EventRedirectParenWiki])
    object MajorEventRedirectWiki extends En.major.EventRedirectWiki()(lp.provide[En.major.EventRedirectWiki])
    object MajorEventWiki extends En.major.EventWiki()(lp.provide[En.major.EventWiki])
    object MajorEventsDisambiguationWiki extends En.major.EventsDisambiguationWiki()(lp.provide[En.major.EventsDisambiguationWiki])
    object MajorEventsParenWiki extends En.major.EventsParenWiki()(lp.provide[En.major.EventsParenWiki])
    object MajorEventsRedirectParenWiki extends En.major.EventsRedirectParenWiki()(lp.provide[En.major.EventsRedirectParenWiki])
    object MajorEventsRedirectWiki extends En.major.EventsRedirectWiki()(lp.provide[En.major.EventsRedirectWiki])
    object MajorEventsWiki extends En.major.EventsWiki()(lp.provide[En.major.EventsWiki])
    object MajorFilmDisambiguationWiki extends En.major.FilmDisambiguationWiki()(lp.provide[En.major.FilmDisambiguationWiki])
    object MajorFilmParenWiki extends En.major.FilmParenWiki()(lp.provide[En.major.FilmParenWiki])
    object MajorFilmRedirectParenWiki extends En.major.FilmRedirectParenWiki()(lp.provide[En.major.FilmRedirectParenWiki])
    object MajorFilmRedirectWiki extends En.major.FilmRedirectWiki()(lp.provide[En.major.FilmRedirectWiki])
    object MajorFilmWiki extends En.major.FilmWiki()(lp.provide[En.major.FilmWiki])
    object MajorGovernmentPoliticalDistrictFree extends En.major.GovernmentPoliticalDistrictFree()(lp.provide[En.major.GovernmentPoliticalDistrictFree])
    object MajorGovernmentPoliticalIdeologyFree extends En.major.GovernmentPoliticalIdeologyFree()(lp.provide[En.major.GovernmentPoliticalIdeologyFree])
    object MajorGovernmentPoliticalPartyFree extends En.major.GovernmentPoliticalPartyFree()(lp.provide[En.major.GovernmentPoliticalPartyFree])
    object MajorJobTitleIesl extends En.major.JobTitleIesl()(lp.provide[En.major.JobTitleIesl])
    object MajorJobtitleIesl extends En.major.JobtitleIesl()(lp.provide[En.major.JobtitleIesl])
    object MajorLawLegalSubjectFree extends En.major.LawLegalSubjectFree()(lp.provide[En.major.LawLegalSubjectFree])
    object MajorLocationAdministrativeDivisionFree extends En.major.LocationAdministrativeDivisionFree()(lp.provide[En.major.LocationAdministrativeDivisionFree])
    object MajorLocationCitytownFree extends En.major.LocationCitytownFree()(lp.provide[En.major.LocationCitytownFree])
    object MajorLocationCountryFree extends En.major.LocationCountryFree()(lp.provide[En.major.LocationCountryFree])
    object MajorLocationDisambiguationWiki extends En.major.LocationDisambiguationWiki()(lp.provide[En.major.LocationDisambiguationWiki])
    object MajorLocationParenWiki extends En.major.LocationParenWiki()(lp.provide[En.major.LocationParenWiki])
    object MajorLocationRedirectParenWiki extends En.major.LocationRedirectParenWiki()(lp.provide[En.major.LocationRedirectParenWiki])
    object MajorLocationRedirectWiki extends En.major.LocationRedirectWiki()(lp.provide[En.major.LocationRedirectWiki])
    object MajorLocationWiki extends En.major.LocationWiki()(lp.provide[En.major.LocationWiki])
    object MajorManMadeThingDisambiguationWiki extends En.major.ManMadeThingDisambiguationWiki()(lp.provide[En.major.ManMadeThingDisambiguationWiki])
    object MajorManMadeThingParenWiki extends En.major.ManMadeThingParenWiki()(lp.provide[En.major.ManMadeThingParenWiki])
    object MajorManMadeThingRedirectParenWiki extends En.major.ManMadeThingRedirectParenWiki()(lp.provide[En.major.ManMadeThingRedirectParenWiki])
    object MajorManMadeThingRedirectWiki extends En.major.ManMadeThingRedirectWiki()(lp.provide[En.major.ManMadeThingRedirectWiki])
    object MajorManMadeThingWiki extends En.major.ManMadeThingWiki()(lp.provide[En.major.ManMadeThingWiki])
    object MajorMan_made_thingDisambiguationWiki extends En.major.Man_made_thingDisambiguationWiki()(lp.provide[En.major.Man_made_thingDisambiguationWiki])
    object MajorMan_made_thingParenWiki extends En.major.Man_made_thingParenWiki()(lp.provide[En.major.Man_made_thingParenWiki])
    object MajorMan_made_thingRedirectParenWiki extends En.major.Man_made_thingRedirectParenWiki()(lp.provide[En.major.Man_made_thingRedirectParenWiki])
    object MajorMan_made_thingRedirectWiki extends En.major.Man_made_thingRedirectWiki()(lp.provide[En.major.Man_made_thingRedirectWiki])
    object MajorMan_made_thingWiki extends En.major.Man_made_thingWiki()(lp.provide[En.major.Man_made_thingWiki])
    object MajorMoneyIesl extends En.major.MoneyIesl()(lp.provide[En.major.MoneyIesl])
    object MajorMonthIesl extends En.major.MonthIesl()(lp.provide[En.major.MonthIesl])
    object MajorOrgSuffixIesl extends En.major.OrgSuffixIesl()(lp.provide[En.major.OrgSuffixIesl])
    object MajorOrganizationDisambiguationWiki extends En.major.OrganizationDisambiguationWiki()(lp.provide[En.major.OrganizationDisambiguationWiki])
    object MajorOrganizationOrganizationFree extends En.major.OrganizationOrganizationFree()(lp.provide[En.major.OrganizationOrganizationFree])
    object MajorOrganizationOrganizationSectorFree extends En.major.OrganizationOrganizationSectorFree()(lp.provide[En.major.OrganizationOrganizationSectorFree])
    object MajorOrganizationParenWiki extends En.major.OrganizationParenWiki()(lp.provide[En.major.OrganizationParenWiki])
    object MajorOrganizationRedirectParenWiki extends En.major.OrganizationRedirectParenWiki()(lp.provide[En.major.OrganizationRedirectParenWiki])
    object MajorOrganizationRedirectWiki extends En.major.OrganizationRedirectWiki()(lp.provide[En.major.OrganizationRedirectWiki])
    object MajorOrganizationWiki extends En.major.OrganizationWiki()(lp.provide[En.major.OrganizationWiki])
    object MajorPeopleCauseOfDeathFree extends En.major.PeopleCauseOfDeathFree()(lp.provide[En.major.PeopleCauseOfDeathFree])
    object MajorPeoplePersonFree extends En.major.PeoplePersonFree()(lp.provide[En.major.PeoplePersonFree])
    object MajorPeopleProfessionFree extends En.major.PeopleProfessionFree()(lp.provide[En.major.PeopleProfessionFree])
    object MajorPersonDisambiguationWiki extends En.major.PersonDisambiguationWiki()(lp.provide[En.major.PersonDisambiguationWiki])
    object MajorPersonFirstHighIesl extends En.major.PersonFirstHighIesl()(lp.provide[En.major.PersonFirstHighIesl])
    object MajorPersonFirstHighestIesl extends En.major.PersonFirstHighestIesl()(lp.provide[En.major.PersonFirstHighestIesl])
    object MajorPersonFirstMediumIesl extends En.major.PersonFirstMediumIesl()(lp.provide[En.major.PersonFirstMediumIesl])
    object MajorPersonHonorificIesl extends En.major.PersonHonorificIesl()(lp.provide[En.major.PersonHonorificIesl])
    object MajorPersonImproperIesl extends En.major.PersonImproperIesl()(lp.provide[En.major.PersonImproperIesl])
    object MajorPersonLastHighIesl extends En.major.PersonLastHighIesl()(lp.provide[En.major.PersonLastHighIesl])
    object MajorPersonLastHighestIesl extends En.major.PersonLastHighestIesl()(lp.provide[En.major.PersonLastHighestIesl])
    object MajorPersonLastMediumIesl extends En.major.PersonLastMediumIesl()(lp.provide[En.major.PersonLastMediumIesl])
    object MajorPersonParenWiki extends En.major.PersonParenWiki()(lp.provide[En.major.PersonParenWiki])
    object MajorPersonRedirectParenWiki extends En.major.PersonRedirectParenWiki()(lp.provide[En.major.PersonRedirectParenWiki])
    object MajorPersonRedirectWiki extends En.major.PersonRedirectWiki()(lp.provide[En.major.PersonRedirectWiki])
    object MajorPersonSuffixIesl extends En.major.PersonSuffixIesl()(lp.provide[En.major.PersonSuffixIesl])
    object MajorPersonWiki extends En.major.PersonWiki()(lp.provide[En.major.PersonWiki])
    object MajorPlaceSuffixIesl extends En.major.PlaceSuffixIesl()(lp.provide[En.major.PlaceSuffixIesl])
    object MajorRedirectDisambiguationWiki extends En.major.RedirectDisambiguationWiki()(lp.provide[En.major.RedirectDisambiguationWiki])
    object MajorReligionReligionFree extends En.major.ReligionReligionFree()(lp.provide[En.major.ReligionReligionFree])
    object MajorReligionReligiousOrganizationFree extends En.major.ReligionReligiousOrganizationFree()(lp.provide[En.major.ReligionReligiousOrganizationFree])
    object MajorSayIesl extends En.major.SayIesl()(lp.provide[En.major.SayIesl])
    object MajorSongDisambiguationWiki extends En.major.SongDisambiguationWiki()(lp.provide[En.major.SongDisambiguationWiki])
    object MajorSongParenWiki extends En.major.SongParenWiki()(lp.provide[En.major.SongParenWiki])
    object MajorSongRedirectParenWiki extends En.major.SongRedirectParenWiki()(lp.provide[En.major.SongRedirectParenWiki])
    object MajorSongRedirectWiki extends En.major.SongRedirectWiki()(lp.provide[En.major.SongRedirectWiki])
    object MajorSongWiki extends En.major.SongWiki()(lp.provide[En.major.SongWiki])
    object MajorSongsDisambiguationWiki extends En.major.SongsDisambiguationWiki()(lp.provide[En.major.SongsDisambiguationWiki])
    object MajorSongsParenWiki extends En.major.SongsParenWiki()(lp.provide[En.major.SongsParenWiki])
    object MajorSongsRedirectParenWiki extends En.major.SongsRedirectParenWiki()(lp.provide[En.major.SongsRedirectParenWiki])
    object MajorSongsRedirectWiki extends En.major.SongsRedirectWiki()(lp.provide[En.major.SongsRedirectWiki])
    object MajorSongsWiki extends En.major.SongsWiki()(lp.provide[En.major.SongsWiki])
    object MajorUkCountyIesl extends En.major.UkCountyIesl()(lp.provide[En.major.UkCountyIesl])
    object MajorUsStateIesl extends En.major.UsStateIesl()(lp.provide[En.major.UsStateIesl])
    object MajorWaterbodiesParenWiki extends En.major.WaterbodiesParenWiki()(lp.provide[En.major.WaterbodiesParenWiki])
    object MajorWaterbodiesRedirectParenWiki extends En.major.WaterbodiesRedirectParenWiki()(lp.provide[En.major.WaterbodiesRedirectParenWiki])
    object MajorWaterbodiesRedirectWiki extends En.major.WaterbodiesRedirectWiki()(lp.provide[En.major.WaterbodiesRedirectWiki])
    object MajorWaterbodiesWiki extends En.major.WaterbodiesWiki()(lp.provide[En.major.WaterbodiesWiki])

    object MajorAnimal extends TrieUnionLexicon("major-animal", MajorAnimalWiki, MajorAnimalRedirectWiki, MajorAnimalRedirectParenWiki, MajorAnimalParenWiki)
    object MajorAnimals extends TrieUnionLexicon("major-animals", MajorAnimalsWiki, MajorAnimalsRedirectWiki, MajorAnimalsRedirectParenWiki, MajorAnimalsParenWiki)
    object MajorBattle extends TrieUnionLexicon("major-battle", MajorBattleWiki, MajorBattleRedirectWiki, MajorBattleRedirectParenWiki, MajorBattleParenWiki, MajorBattleDisambiguationWiki)
    object MajorBattles extends TrieUnionLexicon("major-battles", MajorBattlesWiki, MajorBattlesRedirectWiki, MajorBattlesRedirectParenWiki, MajorBattlesParenWiki, MajorBattlesDisambiguationWiki)
    object MajorBook extends TrieUnionLexicon("major-book", MajorBookWiki, MajorBookRedirectWiki, MajorBookRedirectParenWiki, MajorBookParenWiki, MajorBookDisambiguationWiki)
    object MajorBusiness extends TrieUnionLexicon("major-business", MajorOrgSuffixIesl, MajorBusinessWiki, MajorBusinessRedirectWiki, MajorBusinessRedirectParenWiki, MajorBusinessParenWiki, MajorBusinessDisambiguationWiki, MajorCompanyIesl)
    object MajorCity extends TrieUnionLexicon("major-city", MajorCityWiki, MajorCityRedirectWiki, MajorCityRedirectParenWiki, MajorCityParenWiki, MajorCityIesl, MajorLocationCitytownFree)
    object MajorCompetition extends TrieUnionLexicon("major-competition", MajorCompetitionWiki, MajorCompetitionRedirectWiki, MajorCompetitionRedirectParenWiki, MajorCompetitionParenWiki, MajorCompetitionDisambiguationWiki)
    object MajorCountry extends TrieUnionLexicon("major-country", MajorCountryWiki, MajorCountryRedirectWiki, MajorCountryRedirectParenWiki, MajorCountryParenWiki, MajorCountryIesl, MajorLocationCountryFree)
    object MajorEvent extends TrieUnionLexicon("major-event", MajorEventWiki, MajorEventRedirectWiki, MajorEventRedirectParenWiki, MajorEventParenWiki, MajorEventDisambiguationWiki)
    object MajorEvents extends TrieUnionLexicon("major-events", MajorEventsWiki, MajorEventsRedirectWiki, MajorEventsRedirectParenWiki, MajorEventsParenWiki, MajorEventsDisambiguationWiki)
    object MajorFilm extends TrieUnionLexicon("major-film", MajorFilmWiki, MajorFilmRedirectWiki, MajorFilmRedirectParenWiki, MajorFilmParenWiki, MajorFilmDisambiguationWiki)
    object MajorJobTitle extends TrieUnionLexicon("major-film", MajorBusinessJobTitleFree, MajorJobTitleIesl, MajorJobtitleIesl, MajorPeopleProfessionFree)
    object MajorLocation extends TrieUnionLexicon("major-location", MajorPlaceSuffixIesl, MajorLocationWiki, MajorLocationRedirectWiki, MajorLocationRedirectParenWiki, MajorLocationParenWiki, MajorLocationDisambiguationWiki)
    object MajorManMadeThing extends TrieUnionLexicon("major-man-made-tZhing", MajorManMadeThingWiki, MajorManMadeThingRedirectWiki, MajorManMadeThingRedirectParenWiki, MajorManMadeThingParenWiki, MajorManMadeThingDisambiguationWiki)
    object MajorMan_made_thing extends TrieUnionLexicon("major-man_made_thing", MajorMan_made_thingWiki, MajorMan_made_thingRedirectWiki, MajorMan_made_thingRedirectParenWiki, MajorMan_made_thingParenWiki, MajorMan_made_thingDisambiguationWiki)
    object MajorOrganization extends TrieUnionLexicon("major-organization", MajorGovernmentPoliticalPartyFree, MajorEducationEducationalInstitutionFree, MajorOrgSuffixIesl, MajorOrganizationWiki, MajorOrganizationRedirectWiki, MajorOrganizationRedirectParenWiki, MajorOrganizationParenWiki, MajorOrganizationDisambiguationWiki)
    object MajorPerson extends TrieUnionLexicon("major-person", MajorPersonSuffixIesl, MajorPeoplePersonFree, MajorPersonWiki, MajorPersonRedirectWiki, MajorPersonRedirectParenWiki, MajorPersonParenWiki, MajorPersonDisambiguationWiki)
    object MajorPersonFirst extends TrieUnionLexicon("major-person-first", MajorPersonFirstHighIesl, MajorPersonFirstHighestIesl, MajorPersonFirstMediumIesl)
    object MajorPersonLast extends TrieUnionLexicon("major-person-last", MajorPersonLastHighIesl, MajorPersonLastHighestIesl, MajorPersonLastMediumIesl)
    object MajorSong extends TrieUnionLexicon("major-song", MajorSongWiki, MajorSongRedirectWiki, MajorSongRedirectParenWiki, MajorSongParenWiki, MajorSongDisambiguationWiki)
    object MajorSongs extends TrieUnionLexicon("major-songs", MajorSongsWiki, MajorSongsRedirectWiki, MajorSongsRedirectParenWiki, MajorSongsParenWiki, MajorSongsDisambiguationWiki)
    object MajorState extends TrieUnionLexicon("major-songs", MajorLocationAdministrativeDivisionFree, MajorGovernmentPoliticalDistrictFree, MajorUsStateIesl)
    object MajorWaterbodies extends TrieUnionLexicon("major-waterbodies", MajorWaterbodiesWiki, MajorWaterbodiesRedirectWiki, MajorWaterbodiesRedirectParenWiki, MajorWaterbodiesParenWiki)

    /*
    object MinorBaseFightCrimeTypeFree extends En.minor.BaseFightCrimeTypeFree()(lp.provide[En.minor.BaseFightCrimeTypeFree])
    object MinorBusinessDisambiguationWiki extends En.minor.BusinessDisambiguationWiki()(lp.provide[En.minor.BusinessDisambiguationWiki])
    object MinorBusinessJobTitleFree extends En.minor.BusinessJobTitleFree()(lp.provide[En.minor.BusinessJobTitleFree])
    object MinorBusinessParenWiki extends En.minor.BusinessParenWiki()(lp.provide[En.minor.BusinessParenWiki])
    object MinorBusinessRedirectParenWiki extends En.minor.BusinessRedirectParenWiki()(lp.provide[En.minor.BusinessRedirectParenWiki])
    object MinorBusinessRedirectWiki extends En.minor.BusinessRedirectWiki()(lp.provide[En.minor.BusinessRedirectWiki])
    object MinorBusinessWiki extends En.minor.BusinessWiki()(lp.provide[En.minor.BusinessWiki])
    object MinorCityIesl extends En.minor.CityIesl()(lp.provide[En.minor.CityIesl])
    object MinorCityParenWiki extends En.minor.CityParenWiki()(lp.provide[En.minor.CityParenWiki])
    object MinorCityRedirectParenWiki extends En.minor.CityRedirectParenWiki()(lp.provide[En.minor.CityRedirectParenWiki])
    object MinorCityRedirectWiki extends En.minor.CityRedirectWiki()(lp.provide[En.minor.CityRedirectWiki])
    object MinorCityWiki extends En.minor.CityWiki()(lp.provide[En.minor.CityWiki])
    object MinorCompanyIesl extends En.minor.CompanyIesl()(lp.provide[En.minor.CompanyIesl])
    object MinorCountryIesl extends En.minor.CountryIesl()(lp.provide[En.minor.CountryIesl])
    object MinorCountryParenWiki extends En.minor.CountryParenWiki()(lp.provide[En.minor.CountryParenWiki])
    object MinorCountryRedirectParenWiki extends En.minor.CountryRedirectParenWiki()(lp.provide[En.minor.CountryRedirectParenWiki])
    object MinorCountryRedirectWiki extends En.minor.CountryRedirectWiki()(lp.provide[En.minor.CountryRedirectWiki])
    object MinorCountryWiki extends En.minor.CountryWiki()(lp.provide[En.minor.CountryWiki])
    object MinorEducationEducationalInstitutionFree extends En.minor.EducationEducationalInstitutionFree()(lp.provide[En.minor.EducationEducationalInstitutionFree])
    object MinorGovernmentPoliticalDistrictFree extends En.minor.GovernmentPoliticalDistrictFree()(lp.provide[En.minor.GovernmentPoliticalDistrictFree])
    object MinorGovernmentPoliticalIdeologyFree extends En.minor.GovernmentPoliticalIdeologyFree()(lp.provide[En.minor.GovernmentPoliticalIdeologyFree])
    object MinorGovernmentPoliticalPartyFree extends En.minor.GovernmentPoliticalPartyFree()(lp.provide[En.minor.GovernmentPoliticalPartyFree])
    object MinorJobTitleIesl extends En.minor.JobTitleIesl()(lp.provide[En.minor.JobTitleIesl])
    object MinorJobtitleIesl extends En.minor.JobtitleIesl()(lp.provide[En.minor.JobtitleIesl])
    object MinorLawLegalSubjectFree extends En.minor.LawLegalSubjectFree()(lp.provide[En.minor.LawLegalSubjectFree])
    object MinorLocationAdministrativeDivisionFree extends En.minor.LocationAdministrativeDivisionFree()(lp.provide[En.minor.LocationAdministrativeDivisionFree])
    object MinorLocationCitytownFree extends En.minor.LocationCitytownFree()(lp.provide[En.minor.LocationCitytownFree])
    object MinorLocationCountryFree extends En.minor.LocationCountryFree()(lp.provide[En.minor.LocationCountryFree])
    object MinorLocationDisambiguationWiki extends En.minor.LocationDisambiguationWiki()(lp.provide[En.minor.LocationDisambiguationWiki])
    object MinorLocationParenWiki extends En.minor.LocationParenWiki()(lp.provide[En.minor.LocationParenWiki])
    object MinorLocationRedirectParenWiki extends En.minor.LocationRedirectParenWiki()(lp.provide[En.minor.LocationRedirectParenWiki])
    object MinorLocationRedirectWiki extends En.minor.LocationRedirectWiki()(lp.provide[En.minor.LocationRedirectWiki])
    object MinorLocationWiki extends En.minor.LocationWiki()(lp.provide[En.minor.LocationWiki])
    object MinorOrgSuffixIesl extends En.minor.OrgSuffixIesl()(lp.provide[En.minor.OrgSuffixIesl])
    object MinorOrganizationDisambiguationWiki extends En.minor.OrganizationDisambiguationWiki()(lp.provide[En.minor.OrganizationDisambiguationWiki])
    object MinorOrganizationOrganizationFree extends En.minor.OrganizationOrganizationFree()(lp.provide[En.minor.OrganizationOrganizationFree])
    object MinorOrganizationOrganizationSectorFree extends En.minor.OrganizationOrganizationSectorFree()(lp.provide[En.minor.OrganizationOrganizationSectorFree])
    object MinorOrganizationParenWiki extends En.minor.OrganizationParenWiki()(lp.provide[En.minor.OrganizationParenWiki])
    object MinorOrganizationRedirectParenWiki extends En.minor.OrganizationRedirectParenWiki()(lp.provide[En.minor.OrganizationRedirectParenWiki])
    object MinorOrganizationRedirectWiki extends En.minor.OrganizationRedirectWiki()(lp.provide[En.minor.OrganizationRedirectWiki])
    object MinorOrganizationWiki extends En.minor.OrganizationWiki()(lp.provide[En.minor.OrganizationWiki])
    object MinorPeopleCauseOfDeathFree extends En.minor.PeopleCauseOfDeathFree()(lp.provide[En.minor.PeopleCauseOfDeathFree])
    object MinorPeoplePersonFree extends En.minor.PeoplePersonFree()(lp.provide[En.minor.PeoplePersonFree])
    object MinorPeopleProfessionFree extends En.minor.PeopleProfessionFree()(lp.provide[En.minor.PeopleProfessionFree])
    object MinorPersonDisambiguationWiki extends En.minor.PersonDisambiguationWiki()(lp.provide[En.minor.PersonDisambiguationWiki])
    object MinorPersonFirstHighIesl extends En.minor.PersonFirstHighIesl()(lp.provide[En.minor.PersonFirstHighIesl])
    object MinorPersonFirstHighestIesl extends En.minor.PersonFirstHighestIesl()(lp.provide[En.minor.PersonFirstHighestIesl])
    object MinorPersonFirstMediumIesl extends En.minor.PersonFirstMediumIesl()(lp.provide[En.minor.PersonFirstMediumIesl])
    object MinorPersonHonorificIesl extends En.minor.PersonHonorificIesl()(lp.provide[En.minor.PersonHonorificIesl])
    object MinorPersonLastHighIesl extends En.minor.PersonLastHighIesl()(lp.provide[En.minor.PersonLastHighIesl])
    object MinorPersonLastHighestIesl extends En.minor.PersonLastHighestIesl()(lp.provide[En.minor.PersonLastHighestIesl])
    object MinorPersonLastMediumIesl extends En.minor.PersonLastMediumIesl()(lp.provide[En.minor.PersonLastMediumIesl])
    object MinorPersonParenWiki extends En.minor.PersonParenWiki()(lp.provide[En.minor.PersonParenWiki])
    object MinorPersonRedirectParenWiki extends En.minor.PersonRedirectParenWiki()(lp.provide[En.minor.PersonRedirectParenWiki])
    object MinorPersonRedirectWiki extends En.minor.PersonRedirectWiki()(lp.provide[En.minor.PersonRedirectWiki])
    object MinorPersonWiki extends En.minor.PersonWiki()(lp.provide[En.minor.PersonWiki])
    object MinorReligionReligionFree extends En.minor.ReligionReligionFree()(lp.provide[En.minor.ReligionReligionFree])
    object MinorReligionReligiousOrganizationFree extends En.minor.ReligionReligiousOrganizationFree()(lp.provide[En.minor.ReligionReligiousOrganizationFree])
    */
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

    object MajorBaseFightCrimeTypeFree extends Es.major.BaseFightCrimeTypeFree()(lp.provide[Es.major.BaseFightCrimeTypeFree])
    object MajorBookWiki extends Es.major.BookWiki()(lp.provide[Es.major.BookWiki])
    object MajorBusinessJobTitleFree extends Es.major.BusinessJobTitleFree()(lp.provide[Es.major.BusinessJobTitleFree])
    object MajorBusinessWiki extends Es.major.BusinessWiki()(lp.provide[Es.major.BusinessWiki])
    object MajorContinentsIesl extends Es.major.ContinentsIesl()(lp.provide[Es.major.ContinentsIesl])
    object MajorDayIesl extends Es.major.DayIesl()(lp.provide[Es.major.DayIesl])
    object MajorDemonymIesl extends Es.major.DemonymIesl()(lp.provide[Es.major.DemonymIesl])
    object MajorEducationEducationalInstitutionFree extends Es.major.EducationEducationalInstitutionFree()(lp.provide[Es.major.EducationEducationalInstitutionFree])
    object MajorEventWiki extends Es.major.EventWiki()(lp.provide[Es.major.EventWiki])
    object MajorFilmWiki extends Es.major.FilmWiki()(lp.provide[Es.major.FilmWiki])
    object MajorFirstNameFemaleExtr extends Es.major.FirstNameFemaleExtr()(lp.provide[Es.major.FirstNameFemaleExtr])
    object MajorFirstNameMaleExtr extends Es.major.FirstNameMaleExtr()(lp.provide[Es.major.FirstNameMaleExtr])
    object MajorGovernmentPoliticalDistrictFree extends Es.major.GovernmentPoliticalDistrictFree()(lp.provide[Es.major.GovernmentPoliticalDistrictFree])
    object MajorGovernmentPoliticalIdeologyFree extends Es.major.GovernmentPoliticalIdeologyFree()(lp.provide[Es.major.GovernmentPoliticalIdeologyFree])
    object MajorGovernmentPoliticalPartyFree extends Es.major.GovernmentPoliticalPartyFree()(lp.provide[Es.major.GovernmentPoliticalPartyFree])
    object MajorLastNameExtr extends Es.major.LastNameExtr()(lp.provide[Es.major.LastNameExtr])
    object MajorLawLegalSubjectFree extends Es.major.LawLegalSubjectFree()(lp.provide[Es.major.LawLegalSubjectFree])
    object MajorLocationAdministrativeDivisionFree extends Es.major.LocationAdministrativeDivisionFree()(lp.provide[Es.major.LocationAdministrativeDivisionFree])
    object MajorLocationCityOsmp extends Es.major.LocationCityOsmp()(lp.provide[Es.major.LocationCityOsmp])
    object MajorLocationCityWiki extends Es.major.LocationCityWiki()(lp.provide[Es.major.LocationCityWiki])
    object MajorLocationCitytownFree extends Es.major.LocationCitytownFree()(lp.provide[Es.major.LocationCitytownFree])
    object MajorLocationCountryWiki extends Es.major.LocationCountryWiki()(lp.provide[Es.major.LocationCountryWiki])
    object MajorLocationCountryFree extends Es.major.LocationCountryFree()(lp.provide[Es.major.LocationCountryFree])
    object MajorLocationIesl extends Es.major.LocationIesl()(lp.provide[Es.major.LocationIesl])
    object MajorLocationRedirectWiki extends Es.major.LocationRedirectWiki()(lp.provide[Es.major.LocationRedirectWiki])
    object MajorLocationStateWiki extends Es.major.LocationStateWiki()(lp.provide[Es.major.LocationStateWiki])
    object MajorLocationUndefinedWiki extends Es.major.LocationUndefinedWiki()(lp.provide[Es.major.LocationUndefinedWiki])
    object MajorLocationWiki extends Es.major.LocationWiki()(lp.provide[Es.major.LocationWiki])
    object MajorMiscellaneousIesl extends Es.major.MiscellaneousIesl()(lp.provide[Es.major.MiscellaneousIesl])
    object MajorMonthIesl extends Es.major.MonthIesl()(lp.provide[Es.major.MonthIesl])
    object MajorOrgSuffixIesl extends Es.major.OrgSuffixIesl()(lp.provide[Es.major.OrgSuffixIesl])
    object MajorOrganizationIesl extends Es.major.OrganizationIesl()(lp.provide[Es.major.OrganizationIesl])
    object MajorOrganizationOrganizationFree extends Es.major.OrganizationOrganizationFree()(lp.provide[Es.major.OrganizationOrganizationFree])
    object MajorOrganizationOrganizationSectorFree extends Es.major.OrganizationOrganizationSectorFree()(lp.provide[Es.major.OrganizationOrganizationSectorFree])
    object MajorOrganizationRedirectWiki extends Es.major.OrganizationRedirectWiki()(lp.provide[Es.major.OrganizationRedirectWiki])
    object MajorOrganizationWiki extends Es.major.OrganizationWiki()(lp.provide[Es.major.OrganizationWiki])
    object MajorPeopleCauseOfDeathFree extends Es.major.PeopleCauseOfDeathFree()(lp.provide[Es.major.PeopleCauseOfDeathFree])
    object MajorPeoplePersonFree extends Es.major.PeoplePersonFree()(lp.provide[Es.major.PeoplePersonFree])
    object MajorPeopleProfessionFree extends Es.major.PeopleProfessionFree()(lp.provide[Es.major.PeopleProfessionFree])
    object MajorPersonFirstIesl extends Es.major.PersonFirstIesl()(lp.provide[Es.major.PersonFirstIesl])
    object MajorPersonHonorificIesl extends Es.major.PersonHonorificIesl()(lp.provide[Es.major.PersonHonorificIesl])
    object MajorPersonIesl extends Es.major.PersonIesl()(lp.provide[Es.major.PersonIesl])
    object MajorPersonLastIesl extends Es.major.PersonLastIesl()(lp.provide[Es.major.PersonLastIesl])
    object MajorPersonRedirectWiki extends Es.major.PersonRedirectWiki()(lp.provide[Es.major.PersonRedirectWiki])
    object MajorPersonWiki extends Es.major.PersonWiki()(lp.provide[Es.major.PersonWiki])
    object MajorReligionReligionFree extends Es.major.ReligionReligionFree()(lp.provide[Es.major.ReligionReligionFree])
    object MajorReligionReligiousOrganizationFree extends Es.major.ReligionReligiousOrganizationFree()(lp.provide[Es.major.ReligionReligiousOrganizationFree])

    object MajorPerson extends TrieUnionLexicon("major-person", MajorFirstNameFemaleExtr, MajorFirstNameMaleExtr, MajorLastNameExtr, MajorPeoplePersonFree, MajorPersonFirstIesl, MajorPersonHonorificIesl, MajorPersonIesl, MajorPersonLastIesl, MajorPersonWiki, MajorPersonRedirectWiki)
    object MajorOrganization extends TrieUnionLexicon("major-organization", MajorBusinessWiki, MajorEducationEducationalInstitutionFree, MajorGovernmentPoliticalPartyFree, MajorOrgSuffixIesl, MajorOrganizationIesl, MajorReligionReligiousOrganizationFree, MajorOrganizationRedirectWiki, MajorOrganizationWiki, MajorReligionReligiousOrganizationFree)
    object MajorCity extends TrieUnionLexicon("major-city", MajorLocationCityOsmp, MajorLocationCityWiki, MajorLocationCitytownFree)
    object MajorCountry extends TrieUnionLexicon("major-country", MajorLocationCountryFree, MajorLocationCountryWiki)
    object MajorState extends TrieUnionLexicon("major-state", MajorLocationStateWiki, MajorLocationAdministrativeDivisionFree, MajorGovernmentPoliticalDistrictFree)

    /*
    object MinorBaseFightCrimeTypeFree extends Es.minor.BaseFightCrimeTypeFree()(lp.provide[Es.minor.BaseFightCrimeTypeFree])
    object MinorBusinessJobTitleFree extends Es.minor.BusinessJobTitleFree()(lp.provide[Es.minor.BusinessJobTitleFree])
    object MinorBusinessWiki extends Es.minor.BusinessWiki()(lp.provide[Es.minor.BusinessWiki])
    object MinorEducationEducationalInstitutionFree extends Es.minor.EducationEducationalInstitutionFree()(lp.provide[Es.minor.EducationEducationalInstitutionFree])
    object MinorFirstNameFemaleExtr extends Es.minor.FirstNameFemaleExtr()(lp.provide[Es.minor.FirstNameFemaleExtr])
    object MinorFirstNameMaleExtr extends Es.minor.FirstNameMaleExtr()(lp.provide[Es.minor.FirstNameMaleExtr])
    object MinorGovernmentPoliticalDistrictFree extends Es.minor.GovernmentPoliticalDistrictFree()(lp.provide[Es.minor.GovernmentPoliticalDistrictFree])
    object MinorGovernmentPoliticalIdeologyFree extends Es.minor.GovernmentPoliticalIdeologyFree()(lp.provide[Es.minor.GovernmentPoliticalIdeologyFree])
    object MinorGovernmentPoliticalPartyFree extends Es.minor.GovernmentPoliticalPartyFree()(lp.provide[Es.minor.GovernmentPoliticalPartyFree])
    object MinorLastNameExtr extends Es.minor.LastNameExtr()(lp.provide[Es.minor.LastNameExtr])
    object MinorLawLegalSubjectFree extends Es.minor.LawLegalSubjectFree()(lp.provide[Es.minor.LawLegalSubjectFree])
    object MinorLocationAdministrativeDivisionFree extends Es.minor.LocationAdministrativeDivisionFree()(lp.provide[Es.minor.LocationAdministrativeDivisionFree])
    object MinorLocationCityOsmp extends Es.minor.LocationCityOsmp()(lp.provide[Es.minor.LocationCityOsmp])
    object MinorLocationCityWiki extends Es.minor.LocationCityWiki()(lp.provide[Es.minor.LocationCityWiki])
    object MinorLocationCitytownFree extends Es.minor.LocationCitytownFree()(lp.provide[Es.minor.LocationCitytownFree])
    object MinorLocationCountryWiki extends Es.minor.LocationCountryWiki()(lp.provide[Es.minor.LocationCountryWiki])
    object MinorLocationCountryFree extends Es.minor.LocationCountryFree()(lp.provide[Es.minor.LocationCountryFree])
    object MinorLocationIesl extends Es.minor.LocationIesl()(lp.provide[Es.minor.LocationIesl])
    object MinorLocationRedirectWiki extends Es.minor.LocationRedirectWiki()(lp.provide[Es.minor.LocationRedirectWiki])
    object MinorLocationStateWiki extends Es.minor.LocationStateWiki()(lp.provide[Es.minor.LocationStateWiki])
    object MinorLocationUndefinedWiki extends Es.minor.LocationUndefinedWiki()(lp.provide[Es.minor.LocationUndefinedWiki])
    object MinorLocationWiki extends Es.minor.LocationWiki()(lp.provide[Es.minor.LocationWiki])
    object MinorOrganizationIesl extends Es.minor.OrganizationIesl()(lp.provide[Es.minor.OrganizationIesl])
    object MinorOrganizationOrganizationFree extends Es.minor.OrganizationOrganizationFree()(lp.provide[Es.minor.OrganizationOrganizationFree])
    object MinorOrganizationOrganizationSectorFree extends Es.minor.OrganizationOrganizationSectorFree()(lp.provide[Es.minor.OrganizationOrganizationSectorFree])
    object MinorOrganizationRedirectWiki extends Es.minor.OrganizationRedirectWiki()(lp.provide[Es.minor.OrganizationRedirectWiki])
    object MinorOrganizationWiki extends Es.minor.OrganizationWiki()(lp.provide[Es.minor.OrganizationWiki])
    object MinorPeopleCauseOfDeathFree extends Es.minor.PeopleCauseOfDeathFree()(lp.provide[Es.minor.PeopleCauseOfDeathFree])
    object MinorPeoplePersonFree extends Es.minor.PeoplePersonFree()(lp.provide[Es.minor.PeoplePersonFree])
    object MinorPeopleProfessionFree extends Es.minor.PeopleProfessionFree()(lp.provide[Es.minor.PeopleProfessionFree])
    object MinorPersonFirstIesl extends Es.minor.PersonFirstIesl()(lp.provide[Es.minor.PersonFirstIesl])
    object MinorPersonHonorificIesl extends Es.minor.PersonHonorificIesl()(lp.provide[Es.minor.PersonHonorificIesl])
    object MinorPersonIesl extends Es.minor.PersonIesl()(lp.provide[Es.minor.PersonIesl])
    object MinorPersonLastIesl extends Es.minor.PersonLastIesl()(lp.provide[Es.minor.PersonLastIesl])
    object MinorPersonRedirectWiki extends Es.minor.PersonRedirectWiki()(lp.provide[Es.minor.PersonRedirectWiki])
    object MinorPersonWiki extends Es.minor.PersonWiki()(lp.provide[Es.minor.PersonWiki])
    object MinorReligionReligionFree extends Es.minor.ReligionReligionFree()(lp.provide[Es.minor.ReligionReligionFree])
    object MinorReligionReligiousOrganizationFree extends Es.minor.ReligionReligiousOrganizationFree()(lp.provide[Es.minor.ReligionReligiousOrganizationFree])
    */
  }  

}

