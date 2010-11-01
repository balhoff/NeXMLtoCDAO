package org.nescent.evoinfo

import org.semanticweb.owlapi.model._

object CDAO {
    
    val ONTOLOGY = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl")
    val DATA_MATRIX = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#CharacterStateDataMatrix")
    val OTU = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#TU")
    val CHARACTER = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#Character")
    val HAS_CHARACTER = IRI.create("http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#has_Character")
}