package org.nescent.evoinfo

import org.semanticweb.owlapi.model._

object CDAO {
    
    val ONTOLOGY = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl")
    val DATA_MATRIX = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#CharacterStateDataMatrix")
    val OTU = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#TU")
    val STANDARD_CHARACTER = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#StandardCharacter")
    val CONTINUOUS_CHARACTER = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#ContinuousCharacter")
    val MOLECULAR_CHARACTER = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#MolecularCharacter")
    val COMPOUND_STATE_DATUM = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#CompoundStateDatum")
    val STANDARD_STATE_DATUM = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#StandardStateDatum")
    val CONTINUOUS_STATE_DATUM = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#ContinuousStateDatum")
    val STANDARD_STATE = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#Standard")
    val UNCERTAIN_STATE = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#UncertainStateDomain")
    val POLYMORPHIC_STATE = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#PolymorphicStateDomain")
    
    
    val HAS_TU = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#has_TU")
    val HAS_CHARACTER = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#has_Character")
    val HAS_STANDARD_DATUM = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#has_Standard_Datum")
    val HAS_MOLECULAR_DATUM = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#has_Molecular_Datum")
    val HAS_CONTINUOUS_DATUM = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#has_Continuous_Datum")
    val HAS_COMPOUND_DATUM = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#has_Compound_Datum")
    val BELONGS_TO_CHARACTER = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#belongs_to_Character")
    val BELONGS_TO_TU = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#belongs_to_TU")
    val HAS_FLOAT_VALUE = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#has_Float_Value")
    val HAS_STATE = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#has_State")
    val HAS_COMPOUND_STATE = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#has_Compound_State")
    val HAS_ELEMENT = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#has_Element")
    val HAS_VALUE = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#has_Value")
}