package org.nescent.evoinfo

import org.nexml.model._
import java.io._
import java.net._
import org.semanticweb.owlapi.apibinding._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.vocab._
import scala.collection.JavaConversions._
import org.apache.commons.lang._


object NeXMLtoCDAO {

    val fileIRI = IRI.create("http://example.org/nexmldoc")
    val ontologyManager = OWLManager.createOWLOntologyManager()
    val dataFactory = ontologyManager.getOWLDataFactory()
    val ontology = ontologyManager.createOntology()

    def main(args : Array[String]): Unit = {
        val doc = DocumentFactory.parse(new File(args(0)))
        ontologyManager.setOntologyDocumentIRI(ontology, fileIRI)
        //val importsDeclaration = dataFactory.getOWLImportsDeclaration(CDAO.ONTOLOGY)
        //ontologyManager.applyChange(new AddImport(ontology, importsDeclaration))
        translateNexml(doc)
        ontologyManager.saveOntology(ontology, IRI.create(new File("cdaodata.owl")))
    }

    def translateNexml(doc: Document): Unit = {
        for (otusList <- doc.getOTUsList()) { translateOTUsList(otusList) }
        for (matrix <- doc.getMatrices()) { translateMatrix(matrix) }
    }

    def translateOTUsList(otusList: OTUs): Unit = {
        //this.addClassAssertion(CDAO.OTUS, this.iri(otusList.getId))
        addLabel(otusList)
        for (otu <- otusList.getAllOTUs) { translateOTU(otu) }
    }

    def translateOTU(otu: OTU): Unit = {
        this.addClassAssertion(CDAO.OTU, this.iri(otu.getId))
        this.addLabel(otu)
    }

    def translateMatrix(matrix: Matrix[_]): Unit = {
        matrix match {
        case continuous: ContinuousMatrix => translateContinuousMatrix(continuous)
        case molecular: MolecularMatrix => translateMolecularMatrix(molecular)
        case standard: CategoricalMatrix => translateCategoricalMatrix(standard)
        }
        this.addClassAssertion(CDAO.DATA_MATRIX, this.iri(matrix.getId))
        // NeXML api not reading label?
        this.addLabel(matrix)
    }

    def translateCategoricalMatrix(matrix: CategoricalMatrix): Unit = {
        for (stateSet <- matrix.getCharacterStateSets) {
            //TODO does the state set itself need to be in RDF?
            translateCharacterStateSet(stateSet.getCharacterStates)
        }
        for (character <- matrix.getCharacters) {
            addClassAssertion(CDAO.CATEGORICAL_CHARACTER, iri(character.getId))
            addPropertyAssertion(iri(matrix.getId), CDAO.HAS_CHARACTER, iri(character.getId))
            for (otu <- matrix.getOTUs().getAllOTUs()) {
                val cell = matrix.getCell(otu, character)
                val state = cell.getValue
                if (state != null) {
                    val cellIndividual = dataFactory.getOWLAnonymousIndividual
                    addPropertyAssertionsForCell(cellIndividual, character, otu)
                    state match {
                    case compound: CompoundCharacterState => {
                        addClassAssertion(CDAO.COMPOUND_STATE_DATUM, cellIndividual)
                        addPropertyAssertion(iri(character.getId), CDAO.HAS_COMPOUND_DATUM, cellIndividual)
                        addPropertyAssertion(cellIndividual, CDAO.HAS_COMPOUND_STATE, iri(state.getId))
                    }
                    case standard: CharacterState => { //standard character
                        addClassAssertion(CDAO.STANDARD_STATE_DATUM, cellIndividual)
                        addPropertyAssertion(iri(character.getId), CDAO.HAS_STANDARD_DATUM, cellIndividual)
                        addPropertyAssertion(cellIndividual, CDAO.HAS_STATE, iri(state.getId))
                    }
                    }
                }
            }
        }
    }

    def translateMolecularMatrix(matrix: MolecularMatrix): Unit = {
        for (character <- matrix.getCharacters) {
            addClassAssertion(CDAO.MOLECULAR_CHARACTER, iri(character.getId)) //TODO use more specific class
            addPropertyAssertion(iri(matrix.getId), CDAO.HAS_CHARACTER, iri(character.getId))
            for (otu <- matrix.getOTUs().getAllOTUs()) {
                val cell = matrix.getCell(otu, character)
                if (cell != null) {
                    val cellIndividual = dataFactory.getOWLAnonymousIndividual
                    addPropertyAssertionsForCell(cellIndividual, character, otu)
                    val state = cell.getValue
                    //TODO state
                }
            }
        }
    }

    def translateContinuousMatrix(matrix: ContinuousMatrix): Unit = {
        for (character <- matrix.getCharacters) {
            addClassAssertion(CDAO.CONTINUOUS_CHARACTER, iri(character.getId))
            addPropertyAssertion(iri(matrix.getId), CDAO.HAS_CHARACTER, iri(character.getId))
            for (otu <- matrix.getOTUs().getAllOTUs()) {
                val cell = matrix.getCell(otu, character)
                if (cell != null) {
                    val cellIndividual = dataFactory.getOWLAnonymousIndividual
                    addClassAssertion(CDAO.CONTINUOUS_STATE_DATUM, cellIndividual)
                    addPropertyAssertion(iri(character.getId), CDAO.HAS_CONTINUOUS_DATUM, cellIndividual)
                    addPropertyAssertionsForCell(cellIndividual, character, otu)                    
                    val stateValue = dataFactory.getOWLLiteral(cell.getValue.doubleValue)
                    val valueProperty = dataFactory.getOWLDataProperty(CDAO.HAS_FLOAT_VALUE)
                    //TODO NeXML provides a double but CDAO expects a float
                    val valueAxiom = dataFactory.getOWLDataPropertyAssertionAxiom(valueProperty, cellIndividual, cell.getValue.floatValue)
                    ontologyManager.addAxiom(ontology, valueAxiom)
                }
            }
        }
    }

    def translateCharacterStateSet(stateSet: java.util.Set[CharacterState]): Unit = {
        for (state <- stateSet) {
            state match {
            case compound: CompoundCharacterState => {
                addClassAssertion(CDAO.COMPOUND_STATE, iri(state.getId))
                translateCharacterStateSet(compound.getStates)
            }
            case standard: CharacterState => {
                addClassAssertion(CDAO.STANDARD_STATE, iri(state.getId))
            }
            }
            addLabel(state)
        }
    }

    def addPropertyAssertionsForCell(cell: OWLIndividual, character: Character, otu: OTU): Unit = {
        addPropertyAssertion(cell, CDAO.BELONGS_TO_CHARACTER, iri(character.getId))
        addPropertyAssertion(cell, CDAO.BELONGS_TO_TU, iri(otu.getId))
    }

    def iri(id: String): IRI = {
        IRI.create(this.fileIRI .toString + "#" + id)
    }

    def addClassAssertion(owlClass: IRI, individual: IRI): Unit = {
        val owlIndividual = dataFactory.getOWLNamedIndividual(individual)
        addClassAssertion(owlClass, owlIndividual)
    }

    def addClassAssertion(owlClass: IRI, individual: OWLIndividual): Unit = {
        val owlClassObj = dataFactory.getOWLClass(owlClass)
        ontologyManager.addAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(owlClassObj, individual))
    }

    def addPropertyAssertion(subject: IRI, property: IRI, value: IRI): Unit = {
        val subjectIndividual = dataFactory.getOWLNamedIndividual(subject)
        val valueIndividual = dataFactory.getOWLNamedIndividual(value)
        val objectProperty = dataFactory.getOWLObjectProperty(property)
        addPropertyAssertion(subjectIndividual, objectProperty, valueIndividual)
    }

    def addPropertyAssertion(subject: IRI, property: IRI, value: OWLIndividual): Unit = {
        val subjectIndividual = dataFactory.getOWLNamedIndividual(subject)
        val objectProperty = dataFactory.getOWLObjectProperty(property)
        addPropertyAssertion(subjectIndividual, objectProperty, value)
    }

    def addPropertyAssertion(subject: OWLIndividual, property: IRI, value: IRI): Unit = {
        val valueIndividual = dataFactory.getOWLNamedIndividual(value)
        val objectProperty = dataFactory.getOWLObjectProperty(property)
        addPropertyAssertion(subject, objectProperty, valueIndividual)
    }

    def addPropertyAssertion(subject: OWLIndividual, property: OWLObjectProperty, value: OWLIndividual): Unit = {
        val axiom = dataFactory.getOWLObjectPropertyAssertionAxiom(property, subject, value)
        ontologyManager.addAxiom(ontology, axiom)
    }

    def addLabel(subject: NexmlWritable): Unit = {
        if (StringUtils.isBlank(subject.getLabel)) return
        val rdfsLabel = dataFactory.getOWLAnnotationProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI)
        val labelLiteral = dataFactory.getOWLLiteral(subject.getLabel)
        val annotation = dataFactory.getOWLAnnotation(rdfsLabel, labelLiteral)
        val annotationAxiom = dataFactory.getOWLAnnotationAssertionAxiom(this.iri(subject.getId), annotation)
        ontologyManager.addAxiom(ontology, annotationAxiom)
    }

}
