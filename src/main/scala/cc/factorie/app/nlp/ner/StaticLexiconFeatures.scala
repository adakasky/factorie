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
package cc.factorie.app.nlp.ner

import cc.factorie.app.nlp.Token
import cc.factorie.variable.CategoricalVectorVar
import cc.factorie.app.nlp.lexicon.StaticLexicons
import cc.factorie.app.nlp.lexicon.LexiconsProvider
import cc.factorie.app.nlp.lemma.LowercaseTokenLemma

class StaticLexiconFeatures(lexicon:StaticLexicons, lang: String) extends NerLexiconFeatures {
    //this block serves to initialize all of the lexicons used by the model before processing
  lexicon.synchronized {
    
    if (lang.equals("en") || lang.equals("all")){
      
    	lexicon.iesl.Month.toString()
    	lexicon.iesl.Day.toString()
    	
    	lexicon.iesl.PersonFirst.toString()
    	lexicon.iesl.PersonFirstHigh.toString()
    	lexicon.iesl.PersonFirstHighest.toString()
    	lexicon.iesl.PersonFirstMedium.toString()
    	
    	lexicon.iesl.PersonLast.toString()
    	lexicon.iesl.PersonLastHigh.toString()
    	lexicon.iesl.PersonLastHighest.toString()
    	lexicon.iesl.PersonLastMedium.toString()
    	
    	lexicon.iesl.PersonHonorific.toString()
    	
    	lexicon.iesl.Company.toString()
    	lexicon.iesl.JobTitle.toString()
    	lexicon.iesl.OrgSuffix.toString()
    	
    	lexicon.iesl.Country.toString()
    	lexicon.iesl.City.toString()
    	lexicon.iesl.PlaceSuffix.toString()
    	lexicon.iesl.UsState.toString()
    	lexicon.iesl.Continents.toString()
    	
    	lexicon.wikipedia.Person.toString()
    	lexicon.wikipedia.Event.toString()
    	lexicon.wikipedia.Location.toString()
    	lexicon.wikipedia.Organization.toString()
    	lexicon.wikipedia.ManMadeThing.toString()
    	lexicon.iesl.Demonym.toString()
    	
    	lexicon.wikipedia.Book.toString()
    	lexicon.wikipedia.Business.toString()
    	lexicon.wikipedia.Film.toString()
    	
    	lexicon.wikipedia.LocationAndRedirect.toString()
    	lexicon.wikipedia.PersonAndRedirect.toString()
    	lexicon.wikipedia.OrganizationAndRedirect.toString()
    }
    
    if (lang.equals("es") || lang.equals("all")){

		lexicon.spanish.Continents.toString()
		lexicon.spanish.Month.toString()
      	lexicon.spanish.Day.toString()
      	lexicon.spanish.PersonFirst.toString()
      	lexicon.spanish.PersonLast.toString()
      	lexicon.spanish.Person.toString()
      	lexicon.spanish.PersonHonorific.toString()
      	lexicon.spanish.Location.toString()
      	lexicon.spanish.Organization.toString()
      	lexicon.spanish.Miscellaneous.toString()
      	lexicon.spanish.OrgSuffix.toString()
      	lexicon.spanish.Demonym.toString()
  
      
      	lexicon.spanish.WikiBook.toString()
      	lexicon.spanish.WikiEvent.toString()
      	lexicon.spanish.WikiBusiness.toString()
      	lexicon.spanish.WikiFilm.toString()
		lexicon.spanish.WikiPerson.toString()
      	lexicon.spanish.WikiLocation.toString()
      	lexicon.spanish.WikiOrganization.toString()
      	lexicon.spanish.WikiLocationAndRedirect.toString()
      	lexicon.spanish.WikiPersonAndRedirect.toString()
      	lexicon.spanish.WikiOrganizationAndRedirect.toString()


		lexicon.spanish.MajorBaseFightCrimeTypeFree.toString()
		lexicon.spanish.MajorBookWiki.toString()
		lexicon.spanish.MajorBusinessJobTitleFree.toString()
		lexicon.spanish.MajorBusinessWiki.toString()
		lexicon.spanish.MajorContinentsIesl.toString()
		lexicon.spanish.MajorDayIesl.toString()
		lexicon.spanish.MajorDemonymIesl.toString()
		lexicon.spanish.MajorEducationEducationalInstitutionFree.toString()
		lexicon.spanish.MajorEventWiki.toString()
		lexicon.spanish.MajorFilmWiki.toString()
		lexicon.spanish.MajorFirstNameFemaleExtr.toString()
		lexicon.spanish.MajorFirstNameMaleExtr.toString()
		lexicon.spanish.MajorGovernmentPoliticalDistrictFree.toString()
		lexicon.spanish.MajorGovernmentPoliticalIdeologyFree.toString()
		lexicon.spanish.MajorGovernmentPoliticalPartyFree.toString()
		lexicon.spanish.MajorLastNameExtr.toString()
		lexicon.spanish.MajorLawLegalSubjectFree.toString()
		lexicon.spanish.MajorLocationAdministrativeDivisionFree.toString()
		lexicon.spanish.MajorLocationCityEnOsmp.toString()
		lexicon.spanish.MajorLocationCityEnWiki.toString()
		lexicon.spanish.MajorLocationCityEsOsmp.toString()
		lexicon.spanish.MajorLocationCityEsWiki.toString()
		lexicon.spanish.MajorLocationCitytownFree.toString()
		lexicon.spanish.MajorLocationCountryEnWiki.toString()
		lexicon.spanish.MajorLocationCountryEsWiki.toString()
		lexicon.spanish.MajorLocationCountryFree.toString()
		lexicon.spanish.MajorLocationIesl.toString()
		lexicon.spanish.MajorLocationRedirectWiki.toString()
		lexicon.spanish.MajorLocationStateEnWiki.toString()
		lexicon.spanish.MajorLocationStateEsWiki.toString()
		lexicon.spanish.MajorLocationUndefinedEnWiki.toString()
		lexicon.spanish.MajorLocationUndefinedEsWiki.toString()
		lexicon.spanish.MajorLocationWiki.toString()
		lexicon.spanish.MajorMiscellaneousIesl.toString()
		lexicon.spanish.MajorMonthIesl.toString()
		lexicon.spanish.MajorOrgSuffixIesl.toString()
		lexicon.spanish.MajorOrganizationIesl.toString()
		lexicon.spanish.MajorOrganizationOrganizationFree.toString()
		lexicon.spanish.MajorOrganizationOrganizationSectorFree.toString()
		lexicon.spanish.MajorOrganizationRedirectWiki.toString()
		lexicon.spanish.MajorOrganizationWiki.toString()
		lexicon.spanish.MajorPeopleCauseOfDeathFree.toString()
		lexicon.spanish.MajorPeoplePersonFree.toString()
		lexicon.spanish.MajorPeopleProfessionFree.toString()
		lexicon.spanish.MajorPeopleFirstIesl.toString()
		lexicon.spanish.MajorPeopleHonorificIesl.toString()
		lexicon.spanish.MajorPeopleIesl.toString()
		lexicon.spanish.MajorPeopleLastIesl.toString()
		lexicon.spanish.MajorPeopleRedirectWiki.toString()
		lexicon.spanish.MajorPeopleWiki.toString()
		lexicon.spanish.MajorReligionReligionFree.toString()
		lexicon.spanish.MajorReligionReligiousOrganizationFree.toString()


		lexicon.spanish.MinorBaseFightCrimeTypeFree.toString()
		lexicon.spanish.MinorBusinessJobTitleFree.toString()
		lexicon.spanish.MinorBusinessWiki.toString()
		lexicon.spanish.MinorEducationEducationalInstitutionFree.toString()
		lexicon.spanish.MinorFirstNameFemaleExtr.toString()
		lexicon.spanish.MinorFirstNameMaleExtr.toString()
		lexicon.spanish.MinorGovernmentPoliticalDistrictFree.toString()
		lexicon.spanish.MinorGovernmentPoliticalIdeologyFree.toString()
		lexicon.spanish.MinorGovernmentPoliticalPartyFree.toString()
		lexicon.spanish.MinorLastNameExtr.toString()
		lexicon.spanish.MinorLawLegalSubjectFree.toString()
		lexicon.spanish.MinorLocationAdministrativeDivisionFree.toString()
		lexicon.spanish.MinorLocationCityEnOsmp.toString()
		lexicon.spanish.MinorLocationCityEnWiki.toString()
		lexicon.spanish.MinorLocationCityEsOsmp.toString()
		lexicon.spanish.MinorLocationCityEsWiki.toString()
		lexicon.spanish.MinorLocationCitytownFree.toString()
		lexicon.spanish.MinorLocationCountryEnWiki.toString()
		lexicon.spanish.MinorLocationCountryEsWiki.toString()
		lexicon.spanish.MinorLocationCountryFree.toString()
		lexicon.spanish.MinorLocationIesl.toString()
		lexicon.spanish.MinorLocationRedirectWiki.toString()
		lexicon.spanish.MinorLocationStateEnWiki.toString()
		lexicon.spanish.MinorLocationStateEsWiki.toString()
		lexicon.spanish.MinorLocationUndefinedEnWiki.toString()
		lexicon.spanish.MinorLocationUndefinedEsWiki.toString()
		lexicon.spanish.MinorLocationWiki.toString()
		lexicon.spanish.MinorOrganizationIesl.toString()
		lexicon.spanish.MinorOrganizationOrganizationFree.toString()
		lexicon.spanish.MinorOrganizationOrganizationSectorFree.toString()
		lexicon.spanish.MinorOrganizationRedirectWiki.toString()
		lexicon.spanish.MinorOrganizationWiki.toString()
		lexicon.spanish.MinorPeopleCauseOfDeathFree.toString()
		lexicon.spanish.MinorPeoplePersonFree.toString()
		lexicon.spanish.MinorPeopleProfessionFree.toString()
		lexicon.spanish.MinorPeopleFirstIesl.toString()
		lexicon.spanish.MinorPeopleHonorificIesl.toString()
		lexicon.spanish.MinorPeopleIesl.toString()
		lexicon.spanish.MinorPeopleLastIesl.toString()
		lexicon.spanish.MinorPeopleRedirectWiki.toString()
		lexicon.spanish.MinorPeopleWiki.toString()
		lexicon.spanish.MinorReligionReligionFree.toString()
		lexicon.spanish.MinorReligionReligiousOrganizationFree.toString()
    }
     
  }
  
  
  
  def addLexiconFeatures(tokenSequence: IndexedSeq[Token], vf: (Token => CategoricalVectorVar[String])) {
    
    if (lang.equals("en") || lang.equals("all")){
         
    	lexicon.iesl.Month.tagText(tokenSequence,vf,"MONTH")
    	lexicon.iesl.Day.tagText(tokenSequence,vf,"DAY")
    	
    	lexicon.iesl.PersonFirst.tagText(tokenSequence,vf,"PERSON-FIRST")
    	lexicon.iesl.PersonFirstHigh.tagText(tokenSequence,vf,"PERSON-FIRST-HIGH")
    	lexicon.iesl.PersonFirstHighest.tagText(tokenSequence,vf,"PERSON-FIRST-HIGHEST")
    	lexicon.iesl.PersonFirstMedium.tagText(tokenSequence,vf,"PERSON-FIRST-MEDIUM")
    	
    	lexicon.iesl.PersonLast.tagText(tokenSequence,vf,"PERSON-LAST")
    	lexicon.iesl.PersonLastHigh.tagText(tokenSequence,vf,"PERSON-LAST-HIGH")
    	lexicon.iesl.PersonLastHighest.tagText(tokenSequence,vf,"PERSON-LAST-HIGHEST")
    	lexicon.iesl.PersonLastMedium.tagText(tokenSequence,vf,"PERSON-LAST-MEDIUM")
    	
    	lexicon.iesl.PersonHonorific.tagText(tokenSequence,vf,"PERSON-HONORIFIC")
    	
    	lexicon.iesl.Company.tagText(tokenSequence,vf,"COMPANY")
    	lexicon.iesl.JobTitle.tagText(tokenSequence,vf,"JOB-TITLE")
    	lexicon.iesl.OrgSuffix.tagText(tokenSequence,vf,"ORG-SUFFIX")
    	
    	lexicon.iesl.Country.tagText(tokenSequence,vf,"COUNTRY")
    	lexicon.iesl.City.tagText(tokenSequence,vf,"CITY")
    	lexicon.iesl.PlaceSuffix.tagText(tokenSequence,vf,"PLACE-SUFFIX")
    	lexicon.iesl.UsState.tagText(tokenSequence,vf,"USSTATE")
    	lexicon.iesl.Continents.tagText(tokenSequence,vf,"CONTINENT")
    	
    	lexicon.wikipedia.Person.tagText(tokenSequence,vf,"WIKI-PERSON")
    	lexicon.wikipedia.Event.tagText(tokenSequence,vf,"WIKI-EVENT")
    	lexicon.wikipedia.Location.tagText(tokenSequence,vf,"WIKI-LOCATION")
    	lexicon.wikipedia.Organization.tagText(tokenSequence,vf,"WIKI-ORG")
    	lexicon.wikipedia.ManMadeThing.tagText(tokenSequence,vf,"MANMADE")
    	lexicon.iesl.Demonym.tagText(tokenSequence,vf,"DEMONYM")
    	
    	lexicon.wikipedia.Book.tagText(tokenSequence,vf,"WIKI-BOOK")
    	lexicon.wikipedia.Business.tagText(tokenSequence,vf,"WIKI-BUSINESS")
    	lexicon.wikipedia.Film.tagText(tokenSequence,vf,"WIKI-FILM")
    	
    	lexicon.wikipedia.LocationAndRedirect.tagText(tokenSequence,vf,"WIKI-LOCATION-REDIRECT")
    	lexicon.wikipedia.PersonAndRedirect.tagText(tokenSequence,vf,"WIKI-PERSON-REDIRECT")
    	lexicon.wikipedia.OrganizationAndRedirect.tagText(tokenSequence,vf,"WIKI-ORG-REDIRECT")
    }

    if (lang.equals("es") || lang.equals("all")){
      
      	lexicon.spanish.Continents.tagText(tokenSequence,vf,"CONTINENT")
      	lexicon.spanish.Month.tagText(tokenSequence,vf,"MONTH")
      	lexicon.spanish.Day.tagText(tokenSequence,vf,"DAY")
      	lexicon.spanish.PersonFirst.tagText(tokenSequence,vf,"PERSON-FIRST")
      	lexicon.spanish.PersonLast.tagText(tokenSequence,vf,"PERSON-LAST")
      	lexicon.spanish.Person.tagText(tokenSequence,vf,"PERSON")
      	lexicon.spanish.Location.tagText(tokenSequence,vf,"LOCATION")
      	lexicon.spanish.Organization.tagText(tokenSequence,vf,"ORGANIZATION")
      	lexicon.spanish.Miscellaneous.tagText(tokenSequence,vf,"MISCELLANEOUS")
      	lexicon.spanish.PersonHonorific.tagText(tokenSequence,vf,"PERSON-HONORIFIC")
      	lexicon.spanish.OrgSuffix.tagText(tokenSequence,vf,"ORG-SUFFIX")
      	lexicon.spanish.Demonym.tagText(tokenSequence,vf,"DEMONYM")
  
      
      	lexicon.spanish.WikiBook.tagText(tokenSequence,vf,"WIKI-BOOK")
      	lexicon.spanish.WikiEvent.tagText(tokenSequence,vf,"WIKI-EVENT")
      	lexicon.spanish.WikiFilm.tagText(tokenSequence,vf,"WIKI-FILM")
      	lexicon.spanish.WikiBusiness.tagText(tokenSequence,vf,"WIKI-BUSINESS")
		lexicon.spanish.WikiPerson.tagText(tokenSequence,vf,"WIKI-PERSON")
      	lexicon.spanish.WikiLocation.tagText(tokenSequence,vf,"WIKI-LOCATION")
      	lexicon.spanish.WikiOrganization.tagText(tokenSequence,vf,"WIKI-ORG")
      	lexicon.spanish.WikiLocationAndRedirect.tagText(tokenSequence,vf,"WIKI-LOCATION-REDIRECT")
      	lexicon.spanish.WikiPersonAndRedirect.tagText(tokenSequence,vf,"WIKI-PERSON-REDIRECT")
      	lexicon.spanish.WikiOrganizationAndRedirect.tagText(tokenSequence,vf,"WIKI-ORG-REDIRECT")


		lexicon.spanish.MajorBaseFightCrimeTypeFree.tagText(tokenSequence,vf,"MAJOR-BASE-FIGHT-CRIME-TYPE-FREE")
		lexicon.spanish.MajorBookWiki.tagText(tokenSequence,vf,"MAJOR-BOOK-WIKI")
		lexicon.spanish.MajorBusinessJobTitleFree.tagText(tokenSequence,vf,"MAJOR-BUSINESS-JOB-TITLE-FREE")
		lexicon.spanish.MajorBusinessWiki.tagText(tokenSequence,vf,"MAJOR-BUSINESS-WIKI")
		lexicon.spanish.MajorContinentsIesl.tagText(tokenSequence,vf,"MAJOR-CONTINENTS-IESL")
		lexicon.spanish.MajorDayIesl.tagText(tokenSequence,vf,"MAJOR-DAY-IESL")
		lexicon.spanish.MajorDemonymIesl.tagText(tokenSequence,vf,"MAJOR-DEMONYM-IESL")
		lexicon.spanish.MajorEducationEducationalInstitutionFree.tagText(tokenSequence,vf,"MAJOR-EDUCATION-EDUCATIONAL-INSTITUTION-FREE")
		lexicon.spanish.MajorEventWiki.tagText(tokenSequence,vf,"MAJOR-EVENT-WIKI")
		lexicon.spanish.MajorFilmWiki.tagText(tokenSequence,vf,"MAJOR-FILM-WIKI")
		lexicon.spanish.MajorFirstNameFemaleExtr.tagText(tokenSequence,vf,"MAJOR-FIRST-NAME-FEMALE-EXTR")
		lexicon.spanish.MajorFirstNameMaleExtr.tagText(tokenSequence,vf,"MAJOR-FIRST-NAME-MALE-EXTR")
		lexicon.spanish.MajorGovernmentPoliticalDistrictFree.tagText(tokenSequence,vf,"MAJOR-GOVERNMENT-POLITICAL-DISTRICT-FREE")
		lexicon.spanish.MajorGovernmentPoliticalIdeologyFree.tagText(tokenSequence,vf,"MAJOR-GOVERNMENT-POLITICAL-IDEOLOGY-FREE")
		lexicon.spanish.MajorGovernmentPoliticalPartyFree.tagText(tokenSequence,vf,"MAJOR-GOVERNMENT-POLITICAL-PARTY-FREE")
		lexicon.spanish.MajorLastNameExtr.tagText(tokenSequence,vf,"MAJOR-LAST-NAME-EXTR")
		lexicon.spanish.MajorLawLegalSubjectFree.tagText(tokenSequence,vf,"MAJOR-LAW-LEGAL-SUBJECT-FREE")
		lexicon.spanish.MajorLocationAdministrativeDivisionFree.tagText(tokenSequence,vf,"MAJOR-LOCATION-ADMINISTRATIVE-DIVISION-FREE")
		lexicon.spanish.MajorLocationCityEnOsmp.tagText(tokenSequence,vf,"MAJOR-LOCATION-CITY-EN-OSMP")
		lexicon.spanish.MajorLocationCityEnWiki.tagText(tokenSequence,vf,"MAJOR-LOCATION-CITY-EN-WIKI")
		lexicon.spanish.MajorLocationCityEsOsmp.tagText(tokenSequence,vf,"MAJOR-LOCATION-CITY-ES-OSMP")
		lexicon.spanish.MajorLocationCityEsWiki.tagText(tokenSequence,vf,"MAJOR-LOCATION-CITY-ES-WIKI")
		lexicon.spanish.MajorLocationCitytownFree.tagText(tokenSequence,vf,"MAJOR-LOCATION-CITYTOWN-FREE")
		lexicon.spanish.MajorLocationCountryEnWiki.tagText(tokenSequence,vf,"MAJOR-LOCATION-COUNTRY-EN-WIKI")
		lexicon.spanish.MajorLocationCountryEsWiki.tagText(tokenSequence,vf,"MAJOR-LOCATION-COUNTRY-ES-WIKI")
		lexicon.spanish.MajorLocationCountryFree.tagText(tokenSequence,vf,"MAJOR-LOCATION-COUNTRY-FREE")
		lexicon.spanish.MajorLocationIesl.tagText(tokenSequence,vf,"MAJOR-LOCATION-IESL")
		lexicon.spanish.MajorLocationRedirectWiki.tagText(tokenSequence,vf,"MAJOR-LOCATION-REDIRECT-WIKI")
		lexicon.spanish.MajorLocationStateEnWiki.tagText(tokenSequence,vf,"MAJOR-LOCATION-STATE-EN-WIKI")
		lexicon.spanish.MajorLocationStateEsWiki.tagText(tokenSequence,vf,"MAJOR-LOCATION-STATE-ES-WIKI")
		lexicon.spanish.MajorLocationUndefinedEnWiki.tagText(tokenSequence,vf,"MAJOR-LOCATION-UNDEFINED-EN-WIKI")
		lexicon.spanish.MajorLocationUndefinedEsWiki.tagText(tokenSequence,vf,"MAJOR-LOCATION-UNDEFINED-ES-WIKI")
		lexicon.spanish.MajorLocationWiki.tagText(tokenSequence,vf,"MAJOR-LOCATION-WIKI")
		lexicon.spanish.MajorMiscellaneousIesl.tagText(tokenSequence,vf,"MAJOR-MISCELLANEOUS-IESL")
		lexicon.spanish.MajorMonthIesl.tagText(tokenSequence,vf,"MAJOR-MONTH-IESL")
		lexicon.spanish.MajorOrgSuffixIesl.tagText(tokenSequence,vf,"MAJOR-ORG-SUFFIX-IESL")
		lexicon.spanish.MajorOrganizationIesl.tagText(tokenSequence,vf,"MAJOR-ORGANIZATION-IESL")
		lexicon.spanish.MajorOrganizationOrganizationFree.tagText(tokenSequence,vf,"MAJOR-ORGANIZATION-ORGANIZATION-FREE")
		lexicon.spanish.MajorOrganizationOrganizationSectorFree.tagText(tokenSequence,vf,"MAJOR-ORGANIZATION-ORGANIZATION-SECTOR-FREE")
		lexicon.spanish.MajorOrganizationRedirectWiki.tagText(tokenSequence,vf,"MAJOR-ORGANIZATION-REDIRECT-WIKI")
		lexicon.spanish.MajorOrganizationWiki.tagText(tokenSequence,vf,"MAJOR-ORGANIZATION-WIKI")
		lexicon.spanish.MajorPeopleCauseOfDeathFree.tagText(tokenSequence,vf,"MAJOR-PEOPLE-CAUSE-OF-DEATH-FREE")
		lexicon.spanish.MajorPeoplePersonFree.tagText(tokenSequence,vf,"MAJOR-PEOPLE-PERSON-FREE")
		lexicon.spanish.MajorPeopleProfessionFree.tagText(tokenSequence,vf,"MAJOR-PEOPLE-PROFESSION-FREE")
		lexicon.spanish.MajorPeopleFirstIesl.tagText(tokenSequence,vf,"MAJOR-PERSON-FIRST-IESL")
		lexicon.spanish.MajorPeopleHonorificIesl.tagText(tokenSequence,vf,"MAJOR-PERSON-HONORIFIC-IESL")
		lexicon.spanish.MajorPeopleIesl.tagText(tokenSequence,vf,"MAJOR-PERSON-IESL")
		lexicon.spanish.MajorPeopleLastIesl.tagText(tokenSequence,vf,"MAJOR-PERSON-LAST-IESL")
		lexicon.spanish.MajorPeopleRedirectWiki.tagText(tokenSequence,vf,"MAJOR-PERSON-REDIRECT-WIKI")
		lexicon.spanish.MajorPeopleWiki.tagText(tokenSequence,vf,"MAJOR-PERSON-WIKI")
		lexicon.spanish.MajorReligionReligionFree.tagText(tokenSequence,vf,"MAJOR-RELIGION-RELIGION-FREE")
		lexicon.spanish.MajorReligionReligiousOrganizationFree.tagText(tokenSequence,vf,"MAJOR-RELIGION-RELIGIOUS-ORGANIZATION-FREE")


		lexicon.spanish.MinorBaseFightCrimeTypeFree.tagText(tokenSequence,vf,"MINOR-BASE-FIGHT-CRIME-TYPE-FREE")
		lexicon.spanish.MinorBusinessJobTitleFree.tagText(tokenSequence,vf,"MINOR-BUSINESS-JOB-TITLE-FREE")
		lexicon.spanish.MinorBusinessWiki.tagText(tokenSequence,vf,"MINOR-BUSINESS-WIKI")
		lexicon.spanish.MinorEducationEducationalInstitutionFree.tagText(tokenSequence,vf,"MINOR-EDUCATION-EDUCATIONAL-INSTITUTION-FREE")
		lexicon.spanish.MinorFirstNameFemaleExtr.tagText(tokenSequence,vf,"MINOR-FIRST-NAME-FEMALE-EXTR")
		lexicon.spanish.MinorFirstNameMaleExtr.tagText(tokenSequence,vf,"MINOR-FIRST-NAME-MALE-EXTR")
		lexicon.spanish.MinorGovernmentPoliticalDistrictFree.tagText(tokenSequence,vf,"MINOR-GOVERNMENT-POLITICAL-DISTRICT-FREE")
		lexicon.spanish.MinorGovernmentPoliticalIdeologyFree.tagText(tokenSequence,vf,"MINOR-GOVERNMENT-POLITICAL-IDEOLOGY-FREE")
		lexicon.spanish.MinorGovernmentPoliticalPartyFree.tagText(tokenSequence,vf,"MINOR-GOVERNMENT-POLITICAL-PARTY-FREE")
		lexicon.spanish.MinorLastNameExtr.tagText(tokenSequence,vf,"MINOR-LAST-NAME-EXTR")
		lexicon.spanish.MinorLawLegalSubjectFree.tagText(tokenSequence,vf,"MINOR-LAW-LEGAL-SUBJECT-FREE")
		lexicon.spanish.MinorLocationAdministrativeDivisionFree.tagText(tokenSequence,vf,"MINOR-LOCATION-ADMINISTRATIVE-DIVISION-FREE")
		lexicon.spanish.MinorLocationCityEnOsmp.tagText(tokenSequence,vf,"MINOR-LOCATION-CITY-EN-OSMP")
		lexicon.spanish.MinorLocationCityEnWiki.tagText(tokenSequence,vf,"MINOR-LOCATION-CITY-EN-WIKI")
		lexicon.spanish.MinorLocationCityEsOsmp.tagText(tokenSequence,vf,"MINOR-LOCATION-CITY-ES-OSMP")
		lexicon.spanish.MinorLocationCityEsWiki.tagText(tokenSequence,vf,"MINOR-LOCATION-CITY-ES-WIKI")
		lexicon.spanish.MinorLocationCitytownFree.tagText(tokenSequence,vf,"MINOR-LOCATION-CITYTOWN-FREE")
		lexicon.spanish.MinorLocationCountryEnWiki.tagText(tokenSequence,vf,"MINOR-LOCATION-COUNTRY-EN-WIKI")
		lexicon.spanish.MinorLocationCountryEsWiki.tagText(tokenSequence,vf,"MINOR-LOCATION-COUNTRY-ES-WIKI")
		lexicon.spanish.MinorLocationCountryFree.tagText(tokenSequence,vf,"MINOR-LOCATION-COUNTRY-FREE")
		lexicon.spanish.MinorLocationIesl.tagText(tokenSequence,vf,"MINOR-LOCATION-IESL")
		lexicon.spanish.MinorLocationRedirectWiki.tagText(tokenSequence,vf,"MINOR-LOCATION-REDIRECT-WIKI")
		lexicon.spanish.MinorLocationStateEnWiki.tagText(tokenSequence,vf,"MINOR-LOCATION-STATE-EN-WIKI")
		lexicon.spanish.MinorLocationStateEsWiki.tagText(tokenSequence,vf,"MINOR-LOCATION-STATE-ES-WIKI")
		lexicon.spanish.MinorLocationUndefinedEnWiki.tagText(tokenSequence,vf,"MINOR-LOCATION-UNDEFINED-EN-WIKI")
		lexicon.spanish.MinorLocationUndefinedEsWiki.tagText(tokenSequence,vf,"MINOR-LOCATION-UNDEFINED-ES-WIKI")
		lexicon.spanish.MinorLocationWiki.tagText(tokenSequence,vf,"MINOR-LOCATION-WIKI")
		lexicon.spanish.MinorOrganizationIesl.tagText(tokenSequence,vf,"MINOR-ORGANIZATION-IESL")
		lexicon.spanish.MinorOrganizationOrganizationFree.tagText(tokenSequence,vf,"MINOR-ORGANIZATION-ORGANIZATION-FREE")
		lexicon.spanish.MinorOrganizationOrganizationSectorFree.tagText(tokenSequence,vf,"MINOR-ORGANIZATION-ORGANIZATION-SECTOR-FREE")
		lexicon.spanish.MinorOrganizationRedirectWiki.tagText(tokenSequence,vf,"MINOR-ORGANIZATION-REDIRECT-WIKI")
		lexicon.spanish.MinorOrganizationWiki.tagText(tokenSequence,vf,"MINOR-ORGANIZATION-WIKI")
		lexicon.spanish.MinorPeopleCauseOfDeathFree.tagText(tokenSequence,vf,"MINOR-PEOPLE-CAUSE-OF-DEATH-FREE")
		lexicon.spanish.MinorPeoplePersonFree.tagText(tokenSequence,vf,"MINOR-PEOPLE-PERSON-FREE")
		lexicon.spanish.MinorPeopleProfessionFree.tagText(tokenSequence,vf,"MINOR-PEOPLE-PROFESSION-FREE")
		lexicon.spanish.MinorPeopleFirstIesl.tagText(tokenSequence,vf,"MINOR-PERSON-FIRST-IESL")
		lexicon.spanish.MinorPeopleHonorificIesl.tagText(tokenSequence,vf,"MINOR-PERSON-HONORIFIC-IESL")
		lexicon.spanish.MinorPeopleIesl.tagText(tokenSequence,vf,"MINOR-PERSON-IESL")
		lexicon.spanish.MinorPeopleLastIesl.tagText(tokenSequence,vf,"MINOR-PERSON-LAST-IESL")
		lexicon.spanish.MinorPeopleRedirectWiki.tagText(tokenSequence,vf,"MINOR-PERSON-REDIRECT-WIKI")
		lexicon.spanish.MinorPeopleWiki.tagText(tokenSequence,vf,"MINOR-PERSON-WIKI")
		lexicon.spanish.MinorReligionReligionFree.tagText(tokenSequence,vf,"MINOR-RELIGION-RELIGION-FREE")
		lexicon.spanish.MinorReligionReligiousOrganizationFree.tagText(tokenSequence,vf,"MINOR-RELIGION-RELIGIOUS-ORGANIZATION-FREE")
	}

  }

}

object StaticLexiconFeatures{
  def apply(lang: String = "en"): StaticLexiconFeatures = {
    new StaticLexiconFeatures(new StaticLexicons()(LexiconsProvider.classpath()), lang)
  }
}