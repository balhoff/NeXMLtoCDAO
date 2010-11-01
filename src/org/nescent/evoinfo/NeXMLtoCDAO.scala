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
        //TODO
        matrix match {
            case standard: CategoricalMatrix => translateCategoricalMatrix(standard)
            case continuous: ContinuousMatrix => println("Matched continuous")
        }
        this.addClassAssertion(CDAO.DATA_MATRIX, this.iri(matrix.getId))
        // NeXML api not reading label?
        this.addLabel(matrix)
    }
    
    def translateCategoricalMatrix(matrix: CategoricalMatrix): Unit = {
        for (character <- matrix.getCharacters) {
            addClassAssertion(CDAO.CHARACTER, iri(character.getId))
            addPropertyAssertion(iri(matrix.getId), CDAO.HAS_CHARACTER, iri(character.getId))
        }
    }

    def iri(id: String): IRI = {
        IRI.create(this.fileIRI .toString + "#" + id)
    }

    def addClassAssertion(owlClass: IRI, individual: IRI): Unit = {
        ontologyManager.addAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(dataFactory.getOWLClass(owlClass), dataFactory.getOWLNamedIndividual(individual)))
    }
    
    def addPropertyAssertion(subject: IRI, property: IRI, value: IRI): Unit = {
        val subjectIndividual = dataFactory.getOWLNamedIndividual(subject)
        val valueIndividual = dataFactory.getOWLNamedIndividual(value)
        val objectProperty = dataFactory.getOWLObjectProperty(property)
        val axiom = dataFactory.getOWLObjectPropertyAssertionAxiom(objectProperty, subjectIndividual, valueIndividual)
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
