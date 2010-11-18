package org.nescent.evoinfo

import org.nexml.schema_2009._
import java.io._
import java.net._
import org.semanticweb.owlapi.apibinding._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.io._
import org.semanticweb.owlapi.vocab._
import scala.collection.JavaConversions._
import scala.collection.immutable.StringOps
import scala.collection.mutable._
import org.apache.commons.lang._
import javax.xml.namespace._
import org.w3c.dom._
import org.apache.xmlbeans._

object NeXMLtoCDAO {

    val fileIRI = IRI.create("http://example.org/nexmldoc")
    val ontologyManager = OWLManager.createOWLOntologyManager()
    val dataFactory = ontologyManager.getOWLDataFactory()
    val ontology = ontologyManager.createOntology()
    val availableOtuLists = new HashMap[String, Taxa];

    def main(args : Array[String]) : Unit = {
            val doc = NexmlDocument.Factory.parse(new File(args(0)));
            ontologyManager.setOntologyDocumentIRI(ontology, fileIRI)
            translateNexml(doc.getNexml)
            ontologyManager.saveOntology(ontology, IRI.create(new File("cdaodata.owl")))
    }

    def translateNexml(nexml: Nexml): Unit = {
            translateAnnotated(nexml, dataFactory.getOWLAnonymousIndividual)
            for (otusList <- nexml.getOtusArray()) { translateTaxaBlock(otusList) }
            for (charactersBlock <- nexml.getCharactersArray) {
                charactersBlock match {
                case continuous: ContinuousCells => { translateContinuousCells(continuous) }
                case dna: DnaCells => {}
                case protein: ProteinCells => {}
                case restriction: RestrictionCells => {}
                case rna: RnaCells => {}
                case standard: StandardCells => { translateStandardCells(standard) }
                case continuousSeqs: ContinuousSeqs => {}
                case dnaSeqs: DnaSeqs => {}
                case proteinSeqs: ProteinSeqs => {}
                case restrictionSeqs: RestrictionSeqs => {}
                case rnaSeqs: RnaSeqs => {}
                case standardSeqs: StandardSeqs => {}
                }
            }
            for (treesBlock <- nexml.getTreesArray) { translateTreesBlock(treesBlock) }
    }

    def translateAbstractBlock(block: AbstractBlock, individual: OWLIndividual): Unit = {
            translateLabelled(block, individual)
            addClassAssertion(CDAO.DATA_MATRIX, individual)
            val format = block.getFormat //TODO
            format match {
            case protein: AAFormat => {}
            case continuous: ContinuousFormat => { translateContinuousFormat(continuous, block) }
            case dna: DNAFormat => {}
            case rna: RNAFormat => {}
            case restriction: RestrictionFormat => {}
            case standard: StandardFormat => { translateStandardFormat(standard, block) }
            }
            val otus = availableOtuLists(block.getOtus)
            for (otu <- otus.getOtuArray) {
                addPropertyAssertion(individual, CDAO.HAS_TU, iri(otu.getId))
            }

    }

    def translateAbstractFormat(format: AbstractFormat, individual: OWLIndividual, containingMatrix: AbstractBlock): Unit = {
            translateAnnotated(format, individual)
            for (character <- format.getCharArray) {
                addPropertyAssertion(iri(containingMatrix.getId), CDAO.HAS_CHARACTER, iri(character.getId))
                character match {
                    //TODO
                case continuous: ContinuousChar => { translateContinuousCharacter(continuous) }
                case protein: AAChar => {}
                case dna: DNAChar => {}
                case rna: RNAChar => {}
                case restriction: RestrictionChar => {}
                case standard: StandardChar => { translateStandardCharacter(standard) }
                }
            }
            for (statesSet <- format.getStatesArray) {
                statesSet match { //TODO
                case protein: AAStates => {}
                case dna: DNAStates => {}
                case rna: RNAStates => {}
                case restriction: RestrictionStates => {}
                case standard: StandardStates => { translateStandardStatesSet(standard) }
                }
            }
    }

    def translateAbstractState(state: AbstractState, individual: OWLIndividual): Unit = {
            translateLabelled(state, individual)
    }

    def translateStandardState(state: StandardState): Unit = {
            val individual = dataFactory.getOWLNamedIndividual(iri(state.getId))
            translateAbstractState(state, individual)
            addClassAssertion(CDAO.STANDARD_STATE, individual)
    }

    def translateAbstractStatesSet(states: AbstractStates, individual: OWLIndividual): Unit = {
            translateLabelled(states, individual)
            for (state <- states.getStateArray) {
                state match { //TODO
                case protein: AAState => {}
                case uncertain: AbstractUncertainStateSet => { matchUncertainStateSet(uncertain) }
                case dna: DNAState => {}
                case rna: RNAState => {}
                case restriction: RestrictionState => {} 
                case standard: StandardState => { translateStandardState(standard) }
                }
            }
            for (state <- states.getPolymorphicStateSetArray) {
                matchPolymorphicStateSet(state)
            }
            for (state <- states.getUncertainStateSetArray) {
                matchUncertainStateSet(state)
            }
    }

    def matchUncertainStateSet(states: AbstractUncertainStateSet): Unit = {
            states match { //TODO
            case protein: AAUncertainStateSet => {}
            case polymorphic: AbstractPolymorphicStateSet => { matchPolymorphicStateSet(polymorphic) }
            case dna: DNAUncertainStateSet => {}
            case rna: RNAUncertainStateSet => {}
            case standard: StandardUncertainStateSet => { translateStandardUncertainStateSet(standard) }
            }
    }

    def matchPolymorphicStateSet(states: AbstractPolymorphicStateSet): Unit = {
            //TODO 
            states match {
            case protein: AAPolymorphicStateSet => {}
            case dna: DNAPolymorphicStateSet => {}
            case rna: RNAPolymorphicStateSet => {}
            case standard: StandardPolymorphicStateSet  => { translateStandardPolymorphicStateSet(standard) }
            }
    }

    def translateAbstractPolymorphicStateSet(states: AbstractPolymorphicStateSet, individual: OWLIndividual): Unit = {
            //FIXME CDAO does not have uncertain with polymorphic as subclass but NeXML does
            //translateAbstractUncertainStateSet(states, individual)
            translateAbstractState(states, individual)
            addClassAssertion(CDAO.POLYMORPHIC_STATE, individual)
            for (member <- states.getMemberArray) {
                //FIXME is has_element correct here?
                addPropertyAssertion(individual, CDAO.HAS_ELEMENT, iri(member.getState))
            }
    }

    def translateStandardPolymorphicStateSet(states: StandardPolymorphicStateSet): Unit = {
            val individual = dataFactory.getOWLNamedIndividual(iri(states.getId))
            translateAbstractPolymorphicStateSet(states, individual)
    }

    def translateStandardUncertainStateSet(states: StandardUncertainStateSet): Unit = {
            val individual = dataFactory.getOWLNamedIndividual(iri(states.getId))
            translateAbstractUncertainStateSet(states, individual)
    }

    def translateAbstractUncertainStateSet(states: AbstractUncertainStateSet, individual: OWLIndividual): Unit = {
            translateAbstractState(states, individual)
            addClassAssertion(CDAO.UNCERTAIN_STATE, individual)
            for (member <- states.getMemberArray) {
                addPropertyAssertion(individual, CDAO.HAS, iri(member.getState))
            }
    }

    def translateStandardStatesSet(states: StandardStates): Unit = {
            val individual = dataFactory.getOWLNamedIndividual(iri(states.getId))
            translateAbstractStatesSet(states, individual)
    }

    def translateAbstractCharacter(character: AbstractChar, individual: OWLIndividual): Unit = {
            translateLabelled(character, individual)
    }

    def translateContinuousCharacter(character: ContinuousChar): Unit = {
            val individual = dataFactory.getOWLNamedIndividual(iri(character.getId))
            translateAbstractCharacter(character, individual)
            addClassAssertion(CDAO.CONTINUOUS_CHARACTER, iri(character.getId))
    }

    def translateStandardCharacter(character: StandardChar): Unit = {
            val individual = dataFactory.getOWLNamedIndividual(iri(character.getId))
            translateAbstractCharacter(character, individual)
            addClassAssertion(CDAO.STANDARD_CHARACTER, iri(character.getId))

    }

    def translateContinuousFormat(format: ContinuousFormat, containingMatrix: AbstractBlock): Unit = {
            val individual = dataFactory.getOWLNamedIndividual(iri(format.getId))
            translateAbstractFormat(format, individual, containingMatrix)
    }

    def translateStandardFormat(format: StandardFormat, containingMatrix: AbstractBlock): Unit = {
            val individual = dataFactory.getOWLNamedIndividual(iri(format.getId))
            translateAbstractFormat(format, individual, containingMatrix)
    }

    def translateAbstractCells(cells: AbstractCells, individual: OWLIndividual): Unit = {
            translateAbstractBlock(cells, individual)
            cells.getMatrix match { //TODO
            case protein: AAObsMatrix => {}
            case continuous: ContinuousObsMatrix => { translateContinuousMatrix(continuous) }
            case dna: DNAObsMatrix => {}
            case rna: RNAObsMatrix => {}
            case restriction: RestrictionObsMatrix => {}
            case standard: StandardObsMatrix => { translateStandardMatrix(standard) }
            }
    }

    def translateAbstractMatrix(matrix: AbstractObsMatrix, individual: OWLIndividual): Unit = {
            //TODO
            translateAnnotated(matrix, individual)
            for (row <- matrix.getRowArray) {
                //AAMatrixObsRow, DNAMatrixObsRow, RNAMatrixObsRow, RestrictionMatrixObsRow
                row match {
                case continuous: ContinuousMatrixObsRow => { translateContinuousMatrixRow(continuous) }
                case standard: StandardMatrixObsRow => { translateStandardMatrixRow(standard) }
                }
            }
    }

    def translateAbstractMatrixRow(row: AbstractObsRow, individual: OWLIndividual): Unit = {
            translateLabelled(row, individual)
            for (cell <- row.getCellArray) {
                cell match {
                    //AAObs, DNAObs, RNAObs, RestrictionObs, StandardObs,
                case continuous: ContinuousObs => { translateContinuousMatrixCell(continuous, iri(row.getOtu)) }
                case standard: StandardObs => { translateStandardMatrixCell(standard, iri(row.getOtu)) }
                }
            }
    }

    def translateAbstractMatrixCell(cell: AbstractObs, individual: OWLIndividual, otu: IRI): Unit = {
            translateLabelled(cell, individual)
            //TODO NeXML should have state as IDREF
            val state = cell.getState()
            state match {
            case continuous: ContinuousToken => {
                val valueProperty = dataFactory.getOWLDataProperty(CDAO.HAS_FLOAT_VALUE)
                val valueAxiom = dataFactory.getOWLDataPropertyAssertionAxiom(valueProperty, individual, continuous.getFloatValue)
                ontologyManager.addAxiom(ontology, valueAxiom)
            }
            // this also handles AAObs, DNAObs, RNAObs, RestrictionObs
            case standard: XmlIDREF => { addPropertyAssertion(individual, CDAO.HAS_STATE, iri(cell.getState().getStringValue)) }
            }
            addPropertyAssertion(individual, CDAO.BELONGS_TO_CHARACTER, iri(cell.getChar().getStringValue))
            addPropertyAssertion(individual, CDAO.BELONGS_TO_TU, otu)
    }

    def translateContinuousMatrixCell(cell: ContinuousObs, otu: IRI): Unit = {
            val individual = if (cell.getId == null) {
                dataFactory.getOWLAnonymousIndividual
            } else {
                dataFactory.getOWLNamedIndividual(iri(cell.getId))
            }
            translateAbstractMatrixCell(cell, individual, otu)
            addClassAssertion(CDAO.CONTINUOUS_STATE_DATUM, individual)
            //TODO NeXML should have char as IDREF
            addPropertyAssertion(iri(cell.getChar().getStringValue), CDAO.HAS_CONTINUOUS_DATUM, individual)
    }

    def translateStandardMatrixCell(cell: StandardObs, otu: IRI): Unit = {
            val individual = if (cell.getId == null) {
                dataFactory.getOWLAnonymousIndividual
            } else {
                dataFactory.getOWLNamedIndividual(iri(cell.getId))
            }
            translateAbstractMatrixCell(cell, individual, otu)
            addClassAssertion(CDAO.STANDARD_STATE_DATUM, individual)
            //TODO NeXML should have char as IDREF
            addPropertyAssertion(iri(cell.getChar().getStringValue), CDAO.HAS_STANDARD_DATUM, individual)

    }

    def translateContinuousMatrixRow(row: ContinuousMatrixObsRow): Unit = {
            val individual = dataFactory.getOWLNamedIndividual(iri(row.getId))
            translateAbstractMatrixRow(row, individual)
    }

    def translateStandardMatrixRow(row: StandardMatrixObsRow): Unit = {
            val individual = dataFactory.getOWLNamedIndividual(iri(row.getId))
            translateAbstractMatrixRow(row, individual)
    }

    def translateContinuousMatrix(matrix: ContinuousObsMatrix): Unit = {
            val individual = dataFactory.getOWLNamedIndividual(iri(matrix.getId))
            translateAbstractMatrix(matrix, individual)
    }

    def translateStandardMatrix(matrix: StandardObsMatrix): Unit = {
            val individual = dataFactory.getOWLNamedIndividual(iri(matrix.getId))
            translateAbstractMatrix(matrix, individual)
    }

    def translateContinuousCells(cells: ContinuousCells): Unit = {
            val individual = dataFactory.getOWLNamedIndividual(iri(cells.getId))
            translateAbstractCells(cells, individual)
    }

    def translateStandardCells(cells: StandardCells): Unit = {
            val individual = dataFactory.getOWLNamedIndividual(iri(cells.getId))
            translateAbstractCells(cells, individual)
    }

    def translateTaxaBlock(taxa: Taxa): Unit = {
            val tup = (taxa.getId, taxa) //why can't I use this directly on the next line?
            availableOtuLists += tup
            val individual = dataFactory.getOWLNamedIndividual(iri(taxa.getId))
            translateLabelled(taxa, individual)
            for (taxon <- taxa.getOtuArray) {
                translateTaxon(taxon)
            }
    }

    def translateTaxon(taxon: Taxon): Unit = {
            val individual = dataFactory.getOWLNamedIndividual(iri(taxon.getId))
            translateLabelled(taxon, individual)
            addClassAssertion(CDAO.OTU, individual)
    }

    def translateTreesBlock(trees: Trees): Unit = {
            val individual = dataFactory.getOWLNamedIndividual(iri(trees.getId))
            translateLabelled(trees, individual)
            //TODO
    }

    def translateAnnotated(annotated: Annotated, individual: OWLIndividual): Unit = {
            val individualToUse = if (annotated.isInstanceOf[Nexml]) {
                dataFactory.getOWLNamedIndividual(fileIRI)
            } else {
                individual
            }
            for (meta <- annotated.getMetaArray) {
                meta match {
                case literal: LiteralMeta => { translateLiteralMeta(literal, individualToUse) }
                case resource: ResourceMeta => { translateResourceMeta(resource, individualToUse) }
                }
            }
    }

    def translateLabelled(labelled: Labelled, individual: OWLIndividual): Unit = {
            translateAnnotated(labelled, individual)
            if (StringUtils.isBlank(labelled.getLabel)) return
            val rdfsLabel = dataFactory.getOWLAnnotationProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI)
            val labelLiteral = dataFactory.getOWLLiteral(labelled.getLabel)
            val annotation = dataFactory.getOWLAnnotation(rdfsLabel, labelLiteral)
            if (labelled.getId == null) {
                //TODO warn -- anonymous individuals cannot have annotation properties
            } else {
                val annotationAxiom = dataFactory.getOWLAnnotationAssertionAxiom(iri(labelled.getId), annotation)
                ontologyManager.addAxiom(ontology, annotationAxiom)
            }
    }

    def translateLiteralMeta(meta: LiteralMeta, subject: OWLIndividual): Unit = {
            val content = meta.getContent
            if (content != null) {
                translateLiteralMeta(meta, subject, content)
            }
            // OWLAPI does not seem to support XMLLiterals
            val textContent = getTextContent(meta.getDomNode).trim()
            if (textContent.length > 0) {
                translateLiteralMeta(meta, subject, textContent) 
            }
    }

    def translateLiteralMeta(meta: LiteralMeta, subject: OWLIndividual, value: String): Unit = {
            val property = dataFactory.getOWLDataProperty(iri(meta.getProperty))
            val datatype = if (meta.getDatatype != null) iri(meta.getDatatype) else null
            val axiom = if (datatype == XSDVocabulary.STRING) {
                dataFactory.getOWLDataPropertyAssertionAxiom(property, subject, value)
            } else if (datatype == XSDVocabulary.INT) {
                dataFactory.getOWLDataPropertyAssertionAxiom(property, subject, value.toInt)
            } else if (datatype == XSDVocabulary.LONG) {
                dataFactory.getOWLDataPropertyAssertionAxiom(property, subject, value.toLong)
            } else if (datatype == XSDVocabulary.SHORT) {
                dataFactory.getOWLDataPropertyAssertionAxiom(property, subject, value.toShort)
            } else if (datatype == XSDVocabulary.BOOLEAN) {
                dataFactory.getOWLDataPropertyAssertionAxiom(property, subject, value.toBoolean)
            } else if (datatype == XSDVocabulary.FLOAT) {
                dataFactory.getOWLDataPropertyAssertionAxiom(property, subject, value.toFloat)
            } else if (datatype == XSDVocabulary.DOUBLE) {
                dataFactory.getOWLDataPropertyAssertionAxiom(property, subject, value.toDouble)
            } else {
                dataFactory.getOWLDataPropertyAssertionAxiom(property, subject, value)
            }
            ontologyManager.addAxiom(ontology, axiom)
    }

    def translateResourceMeta(meta: ResourceMeta, subject: OWLIndividual): Unit = {
            val externalResource = meta.getHref
            if (externalResource != null) {
                translateResourceMeta(meta, subject, dataFactory.getOWLNamedIndividual(IRI.create(externalResource)))
            }
            val blankNode = dataFactory.getOWLAnonymousIndividual
            if (!meta.getMetaArray().isEmpty) {
                translateResourceMeta(meta, subject, blankNode)
            }
            for (nestedMeta <- meta.getMetaArray) {
                nestedMeta match {
                case literal: LiteralMeta => { translateLiteralMeta(literal, blankNode) }
                case resource: ResourceMeta => { translateResourceMeta(resource, blankNode) }
                }
            }
    }

    def translateResourceMeta(meta: ResourceMeta, subject: OWLIndividual, value: OWLIndividual): Unit = {
            val property = dataFactory.getOWLObjectProperty(iri(meta.getRel))
            val axiom = dataFactory.getOWLObjectPropertyAssertionAxiom(property, subject, value)
            ontologyManager.addAxiom(ontology, axiom)
    }

    def iri(qname: QName): IRI = {
            return IRI.create(qname.getNamespaceURI + qname.getLocalPart)
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

    def addPropertyAssertion(subject: OWLIndividual, property: IRI, value: OWLIndividual): Unit = {
            val objectProperty = dataFactory.getOWLObjectProperty(property)
            addPropertyAssertion(subject, objectProperty, value)
    }

    def addPropertyAssertion(subject: OWLIndividual, property: OWLObjectProperty, value: OWLIndividual): Unit = {
            val axiom = dataFactory.getOWLObjectPropertyAssertionAxiom(property, subject, value)
            ontologyManager.addAxiom(ontology, axiom)
    }

    def getTextContent(node: Node): String = {
            if (node.getNodeType() == Node.TEXT_NODE) { return (node.asInstanceOf[CharacterData]).getData() }
            val pieces = new StringBuffer()
            val children = node.getChildNodes()
            for (val i <- 0 to (children.getLength - 1)) {
                val child = children.item(i)
                child.getNodeType match {
                case Node.TEXT_NODE => { pieces.append(child.asInstanceOf[CharacterData].getData) }
                case _ => { pieces.append(getTextContent(child)) }
                }
            }
            return pieces.toString();
    }

}
