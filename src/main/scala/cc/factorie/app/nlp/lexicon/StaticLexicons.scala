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

  object english {
    object MajorAnimalParenWiki extends cc.factorie.app.nlp.lexicon.en.major.AnimalParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.AnimalParenWiki])
    object MajorAnimalRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.major.AnimalRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.AnimalRedirectParenWiki])
    object MajorAnimalRedirectWiki extends cc.factorie.app.nlp.lexicon.en.major.AnimalRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.AnimalRedirectWiki])
    object MajorAnimalWiki extends cc.factorie.app.nlp.lexicon.en.major.AnimalWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.AnimalWiki])
    object MajorAnimalsParenWiki extends cc.factorie.app.nlp.lexicon.en.major.AnimalsParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.AnimalsParenWiki])
    object MajorAnimalsRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.major.AnimalsRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.AnimalsRedirectParenWiki])
    object MajorAnimalsRedirectWiki extends cc.factorie.app.nlp.lexicon.en.major.AnimalsRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.AnimalsRedirectWiki])
    object MajorAnimalsWiki extends cc.factorie.app.nlp.lexicon.en.major.AnimalsWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.AnimalsWiki])
    object MajorBaseFightCrimeTypeFree extends cc.factorie.app.nlp.lexicon.en.major.BaseFightCrimeTypeFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.BaseFightCrimeTypeFree])
    object MajorBattleDisambiguationWiki extends cc.factorie.app.nlp.lexicon.en.major.BattleDisambiguationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.BattleDisambiguationWiki])
    object MajorBattleParenWiki extends cc.factorie.app.nlp.lexicon.en.major.BattleParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.BattleParenWiki])
    object MajorBattleRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.major.BattleRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.BattleRedirectParenWiki])
    object MajorBattleRedirectWiki extends cc.factorie.app.nlp.lexicon.en.major.BattleRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.BattleRedirectWiki])
    object MajorBattleWiki extends cc.factorie.app.nlp.lexicon.en.major.BattleWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.BattleWiki])
    object MajorBattlesDisambiguationWiki extends cc.factorie.app.nlp.lexicon.en.major.BattlesDisambiguationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.BattlesDisambiguationWiki])
    object MajorBattlesParenWiki extends cc.factorie.app.nlp.lexicon.en.major.BattlesParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.BattlesParenWiki])
    object MajorBattlesRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.major.BattlesRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.BattlesRedirectParenWiki])
    object MajorBattlesRedirectWiki extends cc.factorie.app.nlp.lexicon.en.major.BattlesRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.BattlesRedirectWiki])
    object MajorBattlesWiki extends cc.factorie.app.nlp.lexicon.en.major.BattlesWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.BattlesWiki])
    object MajorBookDisambiguationWiki extends cc.factorie.app.nlp.lexicon.en.major.BookDisambiguationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.BookDisambiguationWiki])
    object MajorBookParenWiki extends cc.factorie.app.nlp.lexicon.en.major.BookParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.BookParenWiki])
    object MajorBookRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.major.BookRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.BookRedirectParenWiki])
    object MajorBookRedirectWiki extends cc.factorie.app.nlp.lexicon.en.major.BookRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.BookRedirectWiki])
    object MajorBookWiki extends cc.factorie.app.nlp.lexicon.en.major.BookWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.BookWiki])
    object MajorBusinessDisambiguationWiki extends cc.factorie.app.nlp.lexicon.en.major.BusinessDisambiguationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.BusinessDisambiguationWiki])
    object MajorBusinessJobTitleFree extends cc.factorie.app.nlp.lexicon.en.major.BusinessJobTitleFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.BusinessJobTitleFree])
    object MajorBusinessParenWiki extends cc.factorie.app.nlp.lexicon.en.major.BusinessParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.BusinessParenWiki])
    object MajorBusinessRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.major.BusinessRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.BusinessRedirectParenWiki])
    object MajorBusinessRedirectWiki extends cc.factorie.app.nlp.lexicon.en.major.BusinessRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.BusinessRedirectWiki])
    object MajorBusinessWiki extends cc.factorie.app.nlp.lexicon.en.major.BusinessWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.BusinessWiki])
    object MajorCityIesl extends cc.factorie.app.nlp.lexicon.en.major.CityIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.CityIesl])
    object MajorCityParenWiki extends cc.factorie.app.nlp.lexicon.en.major.CityParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.CityParenWiki])
    object MajorCityRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.major.CityRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.CityRedirectParenWiki])
    object MajorCityRedirectWiki extends cc.factorie.app.nlp.lexicon.en.major.CityRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.CityRedirectWiki])
    object MajorCityWiki extends cc.factorie.app.nlp.lexicon.en.major.CityWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.CityWiki])
    object MajorCompanyIesl extends cc.factorie.app.nlp.lexicon.en.major.CompanyIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.CompanyIesl])
    object MajorCompetitionDisambiguationWiki extends cc.factorie.app.nlp.lexicon.en.major.CompetitionDisambiguationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.CompetitionDisambiguationWiki])
    object MajorCompetitionParenWiki extends cc.factorie.app.nlp.lexicon.en.major.CompetitionParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.CompetitionParenWiki])
    object MajorCompetitionRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.major.CompetitionRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.CompetitionRedirectParenWiki])
    object MajorCompetitionRedirectWiki extends cc.factorie.app.nlp.lexicon.en.major.CompetitionRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.CompetitionRedirectWiki])
    object MajorCompetitionWiki extends cc.factorie.app.nlp.lexicon.en.major.CompetitionWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.CompetitionWiki])
    object MajorContinentsIesl extends cc.factorie.app.nlp.lexicon.en.major.ContinentsIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.ContinentsIesl])
    object MajorCountryIesl extends cc.factorie.app.nlp.lexicon.en.major.CountryIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.CountryIesl])
    object MajorCountryParenWiki extends cc.factorie.app.nlp.lexicon.en.major.CountryParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.CountryParenWiki])
    object MajorCountryRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.major.CountryRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.CountryRedirectParenWiki])
    object MajorCountryRedirectWiki extends cc.factorie.app.nlp.lexicon.en.major.CountryRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.CountryRedirectWiki])
    object MajorCountryWiki extends cc.factorie.app.nlp.lexicon.en.major.CountryWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.CountryWiki])
    object MajorDayIesl extends cc.factorie.app.nlp.lexicon.en.major.DayIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.DayIesl])
    object MajorDemonymIesl extends cc.factorie.app.nlp.lexicon.en.major.DemonymIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.DemonymIesl])
    object MajorDemonymsIesl extends cc.factorie.app.nlp.lexicon.en.major.DemonymsIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.DemonymsIesl])
    object MajorEducationEducationalInstitutionFree extends cc.factorie.app.nlp.lexicon.en.major.EducationEducationalInstitutionFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.EducationEducationalInstitutionFree])
    object MajorEventDisambiguationWiki extends cc.factorie.app.nlp.lexicon.en.major.EventDisambiguationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.EventDisambiguationWiki])
    object MajorEventParenWiki extends cc.factorie.app.nlp.lexicon.en.major.EventParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.EventParenWiki])
    object MajorEventRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.major.EventRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.EventRedirectParenWiki])
    object MajorEventRedirectWiki extends cc.factorie.app.nlp.lexicon.en.major.EventRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.EventRedirectWiki])
    object MajorEventWiki extends cc.factorie.app.nlp.lexicon.en.major.EventWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.EventWiki])
    object MajorEventsDisambiguationWiki extends cc.factorie.app.nlp.lexicon.en.major.EventsDisambiguationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.EventsDisambiguationWiki])
    object MajorEventsParenWiki extends cc.factorie.app.nlp.lexicon.en.major.EventsParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.EventsParenWiki])
    object MajorEventsRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.major.EventsRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.EventsRedirectParenWiki])
    object MajorEventsRedirectWiki extends cc.factorie.app.nlp.lexicon.en.major.EventsRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.EventsRedirectWiki])
    object MajorEventsWiki extends cc.factorie.app.nlp.lexicon.en.major.EventsWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.EventsWiki])
    object MajorFilmDisambiguationWiki extends cc.factorie.app.nlp.lexicon.en.major.FilmDisambiguationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.FilmDisambiguationWiki])
    object MajorFilmParenWiki extends cc.factorie.app.nlp.lexicon.en.major.FilmParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.FilmParenWiki])
    object MajorFilmRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.major.FilmRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.FilmRedirectParenWiki])
    object MajorFilmRedirectWiki extends cc.factorie.app.nlp.lexicon.en.major.FilmRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.FilmRedirectWiki])
    object MajorFilmWiki extends cc.factorie.app.nlp.lexicon.en.major.FilmWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.FilmWiki])
    object MajorGovernmentPoliticalDistrictFree extends cc.factorie.app.nlp.lexicon.en.major.GovernmentPoliticalDistrictFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.GovernmentPoliticalDistrictFree])
    object MajorGovernmentPoliticalIdeologyFree extends cc.factorie.app.nlp.lexicon.en.major.GovernmentPoliticalIdeologyFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.GovernmentPoliticalIdeologyFree])
    object MajorGovernmentPoliticalPartyFree extends cc.factorie.app.nlp.lexicon.en.major.GovernmentPoliticalPartyFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.GovernmentPoliticalPartyFree])
    object MajorJobTitleIesl extends cc.factorie.app.nlp.lexicon.en.major.JobTitleIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.JobTitleIesl])
    object MajorJobtitleIesl extends cc.factorie.app.nlp.lexicon.en.major.JobtitleIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.JobtitleIesl])
    object MajorLawLegalSubjectFree extends cc.factorie.app.nlp.lexicon.en.major.LawLegalSubjectFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.LawLegalSubjectFree])
    object MajorLocationAdministrativeDivisionFree extends cc.factorie.app.nlp.lexicon.en.major.LocationAdministrativeDivisionFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.LocationAdministrativeDivisionFree])
    object MajorLocationCitytownFree extends cc.factorie.app.nlp.lexicon.en.major.LocationCitytownFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.LocationCitytownFree])
    object MajorLocationCountryFree extends cc.factorie.app.nlp.lexicon.en.major.LocationCountryFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.LocationCountryFree])
    object MajorLocationDisambiguationWiki extends cc.factorie.app.nlp.lexicon.en.major.LocationDisambiguationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.LocationDisambiguationWiki])
    object MajorLocationParenWiki extends cc.factorie.app.nlp.lexicon.en.major.LocationParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.LocationParenWiki])
    object MajorLocationRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.major.LocationRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.LocationRedirectParenWiki])
    object MajorLocationRedirectWiki extends cc.factorie.app.nlp.lexicon.en.major.LocationRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.LocationRedirectWiki])
    object MajorLocationWiki extends cc.factorie.app.nlp.lexicon.en.major.LocationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.LocationWiki])
    object MajorManMadeThingDisambiguationWiki extends cc.factorie.app.nlp.lexicon.en.major.ManMadeThingDisambiguationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.ManMadeThingDisambiguationWiki])
    object MajorManMadeThingParenWiki extends cc.factorie.app.nlp.lexicon.en.major.ManMadeThingParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.ManMadeThingParenWiki])
    object MajorManMadeThingRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.major.ManMadeThingRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.ManMadeThingRedirectParenWiki])
    object MajorManMadeThingRedirectWiki extends cc.factorie.app.nlp.lexicon.en.major.ManMadeThingRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.ManMadeThingRedirectWiki])
    object MajorManMadeThingWiki extends cc.factorie.app.nlp.lexicon.en.major.ManMadeThingWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.ManMadeThingWiki])
    object MajorMan_made_thingDisambiguationWiki extends cc.factorie.app.nlp.lexicon.en.major.Man_made_thingDisambiguationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.Man_made_thingDisambiguationWiki])
    object MajorMan_made_thingParenWiki extends cc.factorie.app.nlp.lexicon.en.major.Man_made_thingParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.Man_made_thingParenWiki])
    object MajorMan_made_thingRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.major.Man_made_thingRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.Man_made_thingRedirectParenWiki])
    object MajorMan_made_thingRedirectWiki extends cc.factorie.app.nlp.lexicon.en.major.Man_made_thingRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.Man_made_thingRedirectWiki])
    object MajorMan_made_thingWiki extends cc.factorie.app.nlp.lexicon.en.major.Man_made_thingWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.Man_made_thingWiki])
    object MajorMoneyIesl extends cc.factorie.app.nlp.lexicon.en.major.MoneyIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.MoneyIesl])
    object MajorMonthIesl extends cc.factorie.app.nlp.lexicon.en.major.MonthIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.MonthIesl])
    object MajorOrgSuffixIesl extends cc.factorie.app.nlp.lexicon.en.major.OrgSuffixIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.OrgSuffixIesl])
    object MajorOrganizationDisambiguationWiki extends cc.factorie.app.nlp.lexicon.en.major.OrganizationDisambiguationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.OrganizationDisambiguationWiki])
    object MajorOrganizationOrganizationFree extends cc.factorie.app.nlp.lexicon.en.major.OrganizationOrganizationFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.OrganizationOrganizationFree])
    object MajorOrganizationOrganizationSectorFree extends cc.factorie.app.nlp.lexicon.en.major.OrganizationOrganizationSectorFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.OrganizationOrganizationSectorFree])
    object MajorOrganizationParenWiki extends cc.factorie.app.nlp.lexicon.en.major.OrganizationParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.OrganizationParenWiki])
    object MajorOrganizationRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.major.OrganizationRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.OrganizationRedirectParenWiki])
    object MajorOrganizationRedirectWiki extends cc.factorie.app.nlp.lexicon.en.major.OrganizationRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.OrganizationRedirectWiki])
    object MajorOrganizationWiki extends cc.factorie.app.nlp.lexicon.en.major.OrganizationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.OrganizationWiki])
    object MajorPeopleCauseOfDeathFree extends cc.factorie.app.nlp.lexicon.en.major.PeopleCauseOfDeathFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.PeopleCauseOfDeathFree])
    object MajorPeoplePersonFree extends cc.factorie.app.nlp.lexicon.en.major.PeoplePersonFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.PeoplePersonFree])
    object MajorPeopleProfessionFree extends cc.factorie.app.nlp.lexicon.en.major.PeopleProfessionFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.PeopleProfessionFree])
    object MajorPersonDisambiguationWiki extends cc.factorie.app.nlp.lexicon.en.major.PersonDisambiguationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.PersonDisambiguationWiki])
    object MajorPersonFirstHighIesl extends cc.factorie.app.nlp.lexicon.en.major.PersonFirstHighIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.PersonFirstHighIesl])
    object MajorPersonFirstHighestIesl extends cc.factorie.app.nlp.lexicon.en.major.PersonFirstHighestIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.PersonFirstHighestIesl])
    object MajorPersonFirstMediumIesl extends cc.factorie.app.nlp.lexicon.en.major.PersonFirstMediumIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.PersonFirstMediumIesl])
    object MajorPersonHonorificIesl extends cc.factorie.app.nlp.lexicon.en.major.PersonHonorificIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.PersonHonorificIesl])
    object MajorPersonImproperIesl extends cc.factorie.app.nlp.lexicon.en.major.PersonImproperIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.PersonImproperIesl])
    object MajorPersonLastHighIesl extends cc.factorie.app.nlp.lexicon.en.major.PersonLastHighIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.PersonLastHighIesl])
    object MajorPersonLastHighestIesl extends cc.factorie.app.nlp.lexicon.en.major.PersonLastHighestIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.PersonLastHighestIesl])
    object MajorPersonLastMediumIesl extends cc.factorie.app.nlp.lexicon.en.major.PersonLastMediumIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.PersonLastMediumIesl])
    object MajorPersonParenWiki extends cc.factorie.app.nlp.lexicon.en.major.PersonParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.PersonParenWiki])
    object MajorPersonRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.major.PersonRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.PersonRedirectParenWiki])
    object MajorPersonRedirectWiki extends cc.factorie.app.nlp.lexicon.en.major.PersonRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.PersonRedirectWiki])
    object MajorPersonSuffixIesl extends cc.factorie.app.nlp.lexicon.en.major.PersonSuffixIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.PersonSuffixIesl])
    object MajorPersonWiki extends cc.factorie.app.nlp.lexicon.en.major.PersonWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.PersonWiki])
    object MajorPlaceSuffixIesl extends cc.factorie.app.nlp.lexicon.en.major.PlaceSuffixIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.PlaceSuffixIesl])
    object MajorRedirectDisambiguationWiki extends cc.factorie.app.nlp.lexicon.en.major.RedirectDisambiguationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.RedirectDisambiguationWiki])
    object MajorReligionReligionFree extends cc.factorie.app.nlp.lexicon.en.major.ReligionReligionFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.ReligionReligionFree])
    object MajorReligionReligiousOrganizationFree extends cc.factorie.app.nlp.lexicon.en.major.ReligionReligiousOrganizationFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.ReligionReligiousOrganizationFree])
    object MajorSayIesl extends cc.factorie.app.nlp.lexicon.en.major.SayIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.SayIesl])
    object MajorSongDisambiguationWiki extends cc.factorie.app.nlp.lexicon.en.major.SongDisambiguationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.SongDisambiguationWiki])
    object MajorSongParenWiki extends cc.factorie.app.nlp.lexicon.en.major.SongParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.SongParenWiki])
    object MajorSongRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.major.SongRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.SongRedirectParenWiki])
    object MajorSongRedirectWiki extends cc.factorie.app.nlp.lexicon.en.major.SongRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.SongRedirectWiki])
    object MajorSongWiki extends cc.factorie.app.nlp.lexicon.en.major.SongWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.SongWiki])
    object MajorSongsDisambiguationWiki extends cc.factorie.app.nlp.lexicon.en.major.SongsDisambiguationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.SongsDisambiguationWiki])
    object MajorSongsParenWiki extends cc.factorie.app.nlp.lexicon.en.major.SongsParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.SongsParenWiki])
    object MajorSongsRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.major.SongsRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.SongsRedirectParenWiki])
    object MajorSongsRedirectWiki extends cc.factorie.app.nlp.lexicon.en.major.SongsRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.SongsRedirectWiki])
    object MajorSongsWiki extends cc.factorie.app.nlp.lexicon.en.major.SongsWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.SongsWiki])
    object MajorUkCountyIesl extends cc.factorie.app.nlp.lexicon.en.major.UkCountyIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.UkCountyIesl])
    object MajorUsStateIesl extends cc.factorie.app.nlp.lexicon.en.major.UsStateIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.UsStateIesl])
    object MajorWaterbodiesParenWiki extends cc.factorie.app.nlp.lexicon.en.major.WaterbodiesParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.WaterbodiesParenWiki])
    object MajorWaterbodiesRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.major.WaterbodiesRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.WaterbodiesRedirectParenWiki])
    object MajorWaterbodiesRedirectWiki extends cc.factorie.app.nlp.lexicon.en.major.WaterbodiesRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.WaterbodiesRedirectWiki])
    object MajorWaterbodiesWiki extends cc.factorie.app.nlp.lexicon.en.major.WaterbodiesWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.major.WaterbodiesWiki])

    object MinorBaseFightCrimeTypeFree extends cc.factorie.app.nlp.lexicon.en.minor.BaseFightCrimeTypeFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.BaseFightCrimeTypeFree])
    object MinorBusinessDisambiguationWiki extends cc.factorie.app.nlp.lexicon.en.minor.BusinessDisambiguationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.BusinessDisambiguationWiki])
    object MinorBusinessJobTitleFree extends cc.factorie.app.nlp.lexicon.en.minor.BusinessJobTitleFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.BusinessJobTitleFree])
    object MinorBusinessParenWiki extends cc.factorie.app.nlp.lexicon.en.minor.BusinessParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.BusinessParenWiki])
    object MinorBusinessRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.minor.BusinessRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.BusinessRedirectParenWiki])
    object MinorBusinessRedirectWiki extends cc.factorie.app.nlp.lexicon.en.minor.BusinessRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.BusinessRedirectWiki])
    object MinorBusinessWiki extends cc.factorie.app.nlp.lexicon.en.minor.BusinessWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.BusinessWiki])
    object MinorCityIesl extends cc.factorie.app.nlp.lexicon.en.minor.CityIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.CityIesl])
    object MinorCityParenWiki extends cc.factorie.app.nlp.lexicon.en.minor.CityParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.CityParenWiki])
    object MinorCityRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.minor.CityRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.CityRedirectParenWiki])
    object MinorCityRedirectWiki extends cc.factorie.app.nlp.lexicon.en.minor.CityRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.CityRedirectWiki])
    object MinorCityWiki extends cc.factorie.app.nlp.lexicon.en.minor.CityWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.CityWiki])
    object MinorCompanyIesl extends cc.factorie.app.nlp.lexicon.en.minor.CompanyIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.CompanyIesl])
    object MinorCountryIesl extends cc.factorie.app.nlp.lexicon.en.minor.CountryIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.CountryIesl])
    object MinorCountryParenWiki extends cc.factorie.app.nlp.lexicon.en.minor.CountryParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.CountryParenWiki])
    object MinorCountryRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.minor.CountryRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.CountryRedirectParenWiki])
    object MinorCountryRedirectWiki extends cc.factorie.app.nlp.lexicon.en.minor.CountryRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.CountryRedirectWiki])
    object MinorCountryWiki extends cc.factorie.app.nlp.lexicon.en.minor.CountryWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.CountryWiki])
    object MinorEducationEducationalInstitutionFree extends cc.factorie.app.nlp.lexicon.en.minor.EducationEducationalInstitutionFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.EducationEducationalInstitutionFree])
    object MinorGovernmentPoliticalDistrictFree extends cc.factorie.app.nlp.lexicon.en.minor.GovernmentPoliticalDistrictFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.GovernmentPoliticalDistrictFree])
    object MinorGovernmentPoliticalIdeologyFree extends cc.factorie.app.nlp.lexicon.en.minor.GovernmentPoliticalIdeologyFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.GovernmentPoliticalIdeologyFree])
    object MinorGovernmentPoliticalPartyFree extends cc.factorie.app.nlp.lexicon.en.minor.GovernmentPoliticalPartyFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.GovernmentPoliticalPartyFree])
    object MinorJobTitleIesl extends cc.factorie.app.nlp.lexicon.en.minor.JobTitleIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.JobTitleIesl])
    object MinorJobtitleIesl extends cc.factorie.app.nlp.lexicon.en.minor.JobtitleIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.JobtitleIesl])
    object MinorLawLegalSubjectFree extends cc.factorie.app.nlp.lexicon.en.minor.LawLegalSubjectFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.LawLegalSubjectFree])
    object MinorLocationAdministrativeDivisionFree extends cc.factorie.app.nlp.lexicon.en.minor.LocationAdministrativeDivisionFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.LocationAdministrativeDivisionFree])
    object MinorLocationCitytownFree extends cc.factorie.app.nlp.lexicon.en.minor.LocationCitytownFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.LocationCitytownFree])
    object MinorLocationCountryFree extends cc.factorie.app.nlp.lexicon.en.minor.LocationCountryFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.LocationCountryFree])
    object MinorLocationDisambiguationWiki extends cc.factorie.app.nlp.lexicon.en.minor.LocationDisambiguationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.LocationDisambiguationWiki])
    object MinorLocationParenWiki extends cc.factorie.app.nlp.lexicon.en.minor.LocationParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.LocationParenWiki])
    object MinorLocationRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.minor.LocationRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.LocationRedirectParenWiki])
    object MinorLocationRedirectWiki extends cc.factorie.app.nlp.lexicon.en.minor.LocationRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.LocationRedirectWiki])
    object MinorLocationWiki extends cc.factorie.app.nlp.lexicon.en.minor.LocationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.LocationWiki])
    object MinorOrgSuffixIesl extends cc.factorie.app.nlp.lexicon.en.minor.OrgSuffixIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.OrgSuffixIesl])
    object MinorOrganizationDisambiguationWiki extends cc.factorie.app.nlp.lexicon.en.minor.OrganizationDisambiguationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.OrganizationDisambiguationWiki])
    object MinorOrganizationOrganizationFree extends cc.factorie.app.nlp.lexicon.en.minor.OrganizationOrganizationFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.OrganizationOrganizationFree])
    object MinorOrganizationOrganizationSectorFree extends cc.factorie.app.nlp.lexicon.en.minor.OrganizationOrganizationSectorFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.OrganizationOrganizationSectorFree])
    object MinorOrganizationParenWiki extends cc.factorie.app.nlp.lexicon.en.minor.OrganizationParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.OrganizationParenWiki])
    object MinorOrganizationRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.minor.OrganizationRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.OrganizationRedirectParenWiki])
    object MinorOrganizationRedirectWiki extends cc.factorie.app.nlp.lexicon.en.minor.OrganizationRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.OrganizationRedirectWiki])
    object MinorOrganizationWiki extends cc.factorie.app.nlp.lexicon.en.minor.OrganizationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.OrganizationWiki])
    object MinorPeopleCauseOfDeathFree extends cc.factorie.app.nlp.lexicon.en.minor.PeopleCauseOfDeathFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.PeopleCauseOfDeathFree])
    object MinorPeoplePersonFree extends cc.factorie.app.nlp.lexicon.en.minor.PeoplePersonFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.PeoplePersonFree])
    object MinorPeopleProfessionFree extends cc.factorie.app.nlp.lexicon.en.minor.PeopleProfessionFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.PeopleProfessionFree])
    object MinorPersonDisambiguationWiki extends cc.factorie.app.nlp.lexicon.en.minor.PersonDisambiguationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.PersonDisambiguationWiki])
    object MinorPersonFirstHighIesl extends cc.factorie.app.nlp.lexicon.en.minor.PersonFirstHighIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.PersonFirstHighIesl])
    object MinorPersonFirstHighestIesl extends cc.factorie.app.nlp.lexicon.en.minor.PersonFirstHighestIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.PersonFirstHighestIesl])
    object MinorPersonFirstMediumIesl extends cc.factorie.app.nlp.lexicon.en.minor.PersonFirstMediumIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.PersonFirstMediumIesl])
    object MinorPersonHonorificIesl extends cc.factorie.app.nlp.lexicon.en.minor.PersonHonorificIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.PersonHonorificIesl])
    object MinorPersonLastHighIesl extends cc.factorie.app.nlp.lexicon.en.minor.PersonLastHighIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.PersonLastHighIesl])
    object MinorPersonLastHighestIesl extends cc.factorie.app.nlp.lexicon.en.minor.PersonLastHighestIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.PersonLastHighestIesl])
    object MinorPersonLastMediumIesl extends cc.factorie.app.nlp.lexicon.en.minor.PersonLastMediumIesl()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.PersonLastMediumIesl])
    object MinorPersonParenWiki extends cc.factorie.app.nlp.lexicon.en.minor.PersonParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.PersonParenWiki])
    object MinorPersonRedirectParenWiki extends cc.factorie.app.nlp.lexicon.en.minor.PersonRedirectParenWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.PersonRedirectParenWiki])
    object MinorPersonRedirectWiki extends cc.factorie.app.nlp.lexicon.en.minor.PersonRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.PersonRedirectWiki])
    object MinorPersonWiki extends cc.factorie.app.nlp.lexicon.en.minor.PersonWiki()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.PersonWiki])
    object MinorReligionReligionFree extends cc.factorie.app.nlp.lexicon.en.minor.ReligionReligionFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.ReligionReligionFree])
    object MinorReligionReligiousOrganizationFree extends cc.factorie.app.nlp.lexicon.en.minor.ReligionReligiousOrganizationFree()(lp.provide[cc.factorie.app.nlp.lexicon.en.minor.ReligionReligiousOrganizationFree])
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

    object MajorBaseFightCrimeTypeFree extends cc.factorie.app.nlp.lexicon.es.major.BaseFightCrimeTypeFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.BaseFightCrimeTypeFree])
    object MajorBookWiki extends cc.factorie.app.nlp.lexicon.es.major.BookWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.BookWiki])
    object MajorBusinessJobTitleFree extends cc.factorie.app.nlp.lexicon.es.major.BusinessJobTitleFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.BusinessJobTitleFree])
    object MajorBusinessWiki extends cc.factorie.app.nlp.lexicon.es.major.BusinessWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.BusinessWiki])
    object MajorContinentsIesl extends cc.factorie.app.nlp.lexicon.es.major.ContinentsIesl()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.ContinentsIesl])
    object MajorDayIesl extends cc.factorie.app.nlp.lexicon.es.major.DayIesl()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.DayIesl])
    object MajorDemonymIesl extends cc.factorie.app.nlp.lexicon.es.major.DemonymIesl()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.DemonymIesl])
    object MajorEducationEducationalInstitutionFree extends cc.factorie.app.nlp.lexicon.es.major.EducationEducationalInstitutionFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.EducationEducationalInstitutionFree])
    object MajorEventWiki extends cc.factorie.app.nlp.lexicon.es.major.EventWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.EventWiki])
    object MajorFilmWiki extends cc.factorie.app.nlp.lexicon.es.major.FilmWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.FilmWiki])
    object MajorFirstNameFemaleExtr extends cc.factorie.app.nlp.lexicon.es.major.FirstNameFemaleExtr()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.FirstNameFemaleExtr])
    object MajorFirstNameMaleExtr extends cc.factorie.app.nlp.lexicon.es.major.FirstNameMaleExtr()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.FirstNameMaleExtr])
    object MajorGovernmentPoliticalDistrictFree extends cc.factorie.app.nlp.lexicon.es.major.GovernmentPoliticalDistrictFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.GovernmentPoliticalDistrictFree])
    object MajorGovernmentPoliticalIdeologyFree extends cc.factorie.app.nlp.lexicon.es.major.GovernmentPoliticalIdeologyFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.GovernmentPoliticalIdeologyFree])
    object MajorGovernmentPoliticalPartyFree extends cc.factorie.app.nlp.lexicon.es.major.GovernmentPoliticalPartyFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.GovernmentPoliticalPartyFree])
    object MajorLastNameExtr extends cc.factorie.app.nlp.lexicon.es.major.LastNameExtr()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.LastNameExtr])
    object MajorLawLegalSubjectFree extends cc.factorie.app.nlp.lexicon.es.major.LawLegalSubjectFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.LawLegalSubjectFree])
    object MajorLocationAdministrativeDivisionFree extends cc.factorie.app.nlp.lexicon.es.major.LocationAdministrativeDivisionFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.LocationAdministrativeDivisionFree])
    object MajorLocationCityOsmp extends cc.factorie.app.nlp.lexicon.es.major.LocationCityOsmp()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.LocationCityOsmp])
    object MajorLocationCityWiki extends cc.factorie.app.nlp.lexicon.es.major.LocationCityWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.LocationCityWiki])
    object MajorLocationCitytownFree extends cc.factorie.app.nlp.lexicon.es.major.LocationCitytownFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.LocationCitytownFree])
    object MajorLocationCountryWiki extends cc.factorie.app.nlp.lexicon.es.major.LocationCountryWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.LocationCountryWiki])
    object MajorLocationCountryFree extends cc.factorie.app.nlp.lexicon.es.major.LocationCountryFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.LocationCountryFree])
    object MajorLocationIesl extends cc.factorie.app.nlp.lexicon.es.major.LocationIesl()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.LocationIesl])
    object MajorLocationRedirectWiki extends cc.factorie.app.nlp.lexicon.es.major.LocationRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.LocationRedirectWiki])
    object MajorLocationStateWiki extends cc.factorie.app.nlp.lexicon.es.major.LocationStateWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.LocationStateWiki])
    object MajorLocationUndefinedWiki extends cc.factorie.app.nlp.lexicon.es.major.LocationUndefinedWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.LocationUndefinedWiki])
    object MajorLocationWiki extends cc.factorie.app.nlp.lexicon.es.major.LocationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.LocationWiki])
    object MajorMiscellaneousIesl extends cc.factorie.app.nlp.lexicon.es.major.MiscellaneousIesl()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.MiscellaneousIesl])
    object MajorMonthIesl extends cc.factorie.app.nlp.lexicon.es.major.MonthIesl()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.MonthIesl])
    object MajorOrgSuffixIesl extends cc.factorie.app.nlp.lexicon.es.major.OrgSuffixIesl()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.OrgSuffixIesl])
    object MajorOrganizationIesl extends cc.factorie.app.nlp.lexicon.es.major.OrganizationIesl()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.OrganizationIesl])
    object MajorOrganizationOrganizationFree extends cc.factorie.app.nlp.lexicon.es.major.OrganizationOrganizationFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.OrganizationOrganizationFree])
    object MajorOrganizationOrganizationSectorFree extends cc.factorie.app.nlp.lexicon.es.major.OrganizationOrganizationSectorFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.OrganizationOrganizationSectorFree])
    object MajorOrganizationRedirectWiki extends cc.factorie.app.nlp.lexicon.es.major.OrganizationRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.OrganizationRedirectWiki])
    object MajorOrganizationWiki extends cc.factorie.app.nlp.lexicon.es.major.OrganizationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.OrganizationWiki])
    object MajorPeopleCauseOfDeathFree extends cc.factorie.app.nlp.lexicon.es.major.PeopleCauseOfDeathFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.PeopleCauseOfDeathFree])
    object MajorPeoplePersonFree extends cc.factorie.app.nlp.lexicon.es.major.PeoplePersonFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.PeoplePersonFree])
    object MajorPeopleProfessionFree extends cc.factorie.app.nlp.lexicon.es.major.PeopleProfessionFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.PeopleProfessionFree])
    object MajorPersonFirstIesl extends cc.factorie.app.nlp.lexicon.es.major.PersonFirstIesl()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.PersonFirstIesl])
    object MajorPersonHonorificIesl extends cc.factorie.app.nlp.lexicon.es.major.PersonHonorificIesl()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.PersonHonorificIesl])
    object MajorPersonIesl extends cc.factorie.app.nlp.lexicon.es.major.PersonIesl()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.PersonIesl])
    object MajorPersonLastIesl extends cc.factorie.app.nlp.lexicon.es.major.PersonLastIesl()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.PersonLastIesl])
    object MajorPersonRedirectWiki extends cc.factorie.app.nlp.lexicon.es.major.PersonRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.PersonRedirectWiki])
    object MajorPersonWiki extends cc.factorie.app.nlp.lexicon.es.major.PersonWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.PersonWiki])
    object MajorReligionReligionFree extends cc.factorie.app.nlp.lexicon.es.major.ReligionReligionFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.ReligionReligionFree])
    object MajorReligionReligiousOrganizationFree extends cc.factorie.app.nlp.lexicon.es.major.ReligionReligiousOrganizationFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.major.ReligionReligiousOrganizationFree])

    object MinorBaseFightCrimeTypeFree extends cc.factorie.app.nlp.lexicon.es.minor.BaseFightCrimeTypeFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.BaseFightCrimeTypeFree])
    object MinorBusinessJobTitleFree extends cc.factorie.app.nlp.lexicon.es.minor.BusinessJobTitleFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.BusinessJobTitleFree])
    object MinorBusinessWiki extends cc.factorie.app.nlp.lexicon.es.minor.BusinessWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.BusinessWiki])
    object MinorEducationEducationalInstitutionFree extends cc.factorie.app.nlp.lexicon.es.minor.EducationEducationalInstitutionFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.EducationEducationalInstitutionFree])
    object MinorFirstNameFemaleExtr extends cc.factorie.app.nlp.lexicon.es.minor.FirstNameFemaleExtr()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.FirstNameFemaleExtr])
    object MinorFirstNameMaleExtr extends cc.factorie.app.nlp.lexicon.es.minor.FirstNameMaleExtr()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.FirstNameMaleExtr])
    object MinorGovernmentPoliticalDistrictFree extends cc.factorie.app.nlp.lexicon.es.minor.GovernmentPoliticalDistrictFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.GovernmentPoliticalDistrictFree])
    object MinorGovernmentPoliticalIdeologyFree extends cc.factorie.app.nlp.lexicon.es.minor.GovernmentPoliticalIdeologyFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.GovernmentPoliticalIdeologyFree])
    object MinorGovernmentPoliticalPartyFree extends cc.factorie.app.nlp.lexicon.es.minor.GovernmentPoliticalPartyFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.GovernmentPoliticalPartyFree])
    object MinorLastNameExtr extends cc.factorie.app.nlp.lexicon.es.minor.LastNameExtr()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.LastNameExtr])
    object MinorLawLegalSubjectFree extends cc.factorie.app.nlp.lexicon.es.minor.LawLegalSubjectFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.LawLegalSubjectFree])
    object MinorLocationAdministrativeDivisionFree extends cc.factorie.app.nlp.lexicon.es.minor.LocationAdministrativeDivisionFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.LocationAdministrativeDivisionFree])
    object MinorLocationCityOsmp extends cc.factorie.app.nlp.lexicon.es.minor.LocationCityOsmp()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.LocationCityOsmp])
    object MinorLocationCityWiki extends cc.factorie.app.nlp.lexicon.es.minor.LocationCityWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.LocationCityWiki])
    object MinorLocationCitytownFree extends cc.factorie.app.nlp.lexicon.es.minor.LocationCitytownFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.LocationCitytownFree])
    object MinorLocationCountryWiki extends cc.factorie.app.nlp.lexicon.es.minor.LocationCountryWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.LocationCountryWiki])
    object MinorLocationCountryFree extends cc.factorie.app.nlp.lexicon.es.minor.LocationCountryFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.LocationCountryFree])
    object MinorLocationIesl extends cc.factorie.app.nlp.lexicon.es.minor.LocationIesl()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.LocationIesl])
    object MinorLocationRedirectWiki extends cc.factorie.app.nlp.lexicon.es.minor.LocationRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.LocationRedirectWiki])
    object MinorLocationStateWiki extends cc.factorie.app.nlp.lexicon.es.minor.LocationStateWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.LocationStateWiki])
    object MinorLocationUndefinedWiki extends cc.factorie.app.nlp.lexicon.es.minor.LocationUndefinedWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.LocationUndefinedWiki])
    object MinorLocationWiki extends cc.factorie.app.nlp.lexicon.es.minor.LocationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.LocationWiki])
    object MinorOrganizationIesl extends cc.factorie.app.nlp.lexicon.es.minor.OrganizationIesl()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.OrganizationIesl])
    object MinorOrganizationOrganizationFree extends cc.factorie.app.nlp.lexicon.es.minor.OrganizationOrganizationFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.OrganizationOrganizationFree])
    object MinorOrganizationOrganizationSectorFree extends cc.factorie.app.nlp.lexicon.es.minor.OrganizationOrganizationSectorFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.OrganizationOrganizationSectorFree])
    object MinorOrganizationRedirectWiki extends cc.factorie.app.nlp.lexicon.es.minor.OrganizationRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.OrganizationRedirectWiki])
    object MinorOrganizationWiki extends cc.factorie.app.nlp.lexicon.es.minor.OrganizationWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.OrganizationWiki])
    object MinorPeopleCauseOfDeathFree extends cc.factorie.app.nlp.lexicon.es.minor.PeopleCauseOfDeathFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.PeopleCauseOfDeathFree])
    object MinorPeoplePersonFree extends cc.factorie.app.nlp.lexicon.es.minor.PeoplePersonFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.PeoplePersonFree])
    object MinorPeopleProfessionFree extends cc.factorie.app.nlp.lexicon.es.minor.PeopleProfessionFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.PeopleProfessionFree])
    object MinorPersonFirstIesl extends cc.factorie.app.nlp.lexicon.es.minor.PersonFirstIesl()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.PersonFirstIesl])
    object MinorPersonHonorificIesl extends cc.factorie.app.nlp.lexicon.es.minor.PersonHonorificIesl()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.PersonHonorificIesl])
    object MinorPersonIesl extends cc.factorie.app.nlp.lexicon.es.minor.PersonIesl()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.PersonIesl])
    object MinorPersonLastIesl extends cc.factorie.app.nlp.lexicon.es.minor.PersonLastIesl()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.PersonLastIesl])
    object MinorPersonRedirectWiki extends cc.factorie.app.nlp.lexicon.es.minor.PersonRedirectWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.PersonRedirectWiki])
    object MinorPersonWiki extends cc.factorie.app.nlp.lexicon.es.minor.PersonWiki()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.PersonWiki])
    object MinorReligionReligionFree extends cc.factorie.app.nlp.lexicon.es.minor.ReligionReligionFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.ReligionReligionFree])
    object MinorReligionReligiousOrganizationFree extends cc.factorie.app.nlp.lexicon.es.minor.ReligionReligiousOrganizationFree()(lp.provide[cc.factorie.app.nlp.lexicon.es.minor.ReligionReligiousOrganizationFree])
  }  

}

