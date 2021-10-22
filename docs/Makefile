DOC = GodXCoin-Monitoring

SRCDIR1 = ../tbco-monitoring/src
SRCDIR2 = ../contra-tracer/src
SRCDIR3 = ../plugins
SOURCES = Code.lhs \
					$(SRCDIR1)/Bcc/BM/Configuration.lhs \
					$(SRCDIR1)/Bcc/BM/Configuration/Model.lhs \
					$(SRCDIR1)/Bcc/BM/Configuration/Static.lhs \
					$(SRCDIR1)/Bcc/BM/Counters.lhs \
					$(SRCDIR1)/Bcc/BM/Counters/Common.lhs \
					$(SRCDIR1)/Bcc/BM/Counters/Dummy.lhs \
					$(SRCDIR1)/Bcc/BM/Counters/Linux.lhs \
					$(SRCDIR1)/Bcc/BM/Data/Aggregated.lhs \
					$(SRCDIR1)/Bcc/BM/Data/AggregatedKind.lhs \
					$(SRCDIR1)/Bcc/BM/Data/Backend.lhs \
					$(SRCDIR1)/Bcc/BM/Data/BackendKind.lhs \
					$(SRCDIR1)/Bcc/BM/Data/Configuration.lhs \
					$(SRCDIR1)/Bcc/BM/Data/Counter.lhs \
					$(SRCDIR1)/Bcc/BM/Data/LogItem.lhs \
					$(SRCDIR1)/Bcc/BM/Data/MonitoringEval.lhs \
					$(SRCDIR1)/Bcc/BM/Data/Observable.lhs \
					$(SRCDIR1)/Bcc/BM/Data/Output.lhs \
					$(SRCDIR1)/Bcc/BM/Data/Rotation.lhs \
					$(SRCDIR1)/Bcc/BM/Data/Severity.lhs \
					$(SRCDIR1)/Bcc/BM/Data/SubTrace.lhs \
					$(SRCDIR1)/Bcc/BM/Data/Trace.lhs \
					$(SRCDIR1)/Bcc/BM/Data/Tracer.lhs \
					$(SRCDIR1)/Bcc/BM/Observer/Monadic.lhs \
					$(SRCDIR1)/Bcc/BM/Observer/STM.lhs \
					$(SRCDIR3)/backend-aggregation/src/Bcc/BM/Backend/Aggregation.lhs \
					$(SRCDIR3)/backend-editor/src/Bcc/BM/Backend/Editor.lhs \
					$(SRCDIR3)/backend-ekg/src/Bcc/BM/Backend/EKGView.lhs \
					$(SRCDIR3)/backend-graylog/src/Bcc/BM/Backend/Graylog.lhs \
					$(SRCDIR1)/Bcc/BM/Backend/Log.lhs \
					$(SRCDIR1)/Bcc/BM/Backend/LogBuffer.lhs \
					$(SRCDIR3)/backend-monitoring/src/Bcc/BM/Backend/Monitoring.lhs \
					$(SRCDIR3)/backend-prometheus/src/Bcc/BM/Backend/Prometheus.lhs \
					$(SRCDIR1)/Bcc/BM/Backend/Switchboard.lhs \
					$(SRCDIR1)/Bcc/BM/Backend/TraceAcceptor.lhs \
					$(SRCDIR1)/Bcc/BM/Backend/TraceForwarder.lhs \
					$(SRCDIR1)/Bcc/BM/Rotator.lhs \
					$(SRCDIR1)/Bcc/BM/Setup.lhs \
					$(SRCDIR1)/Bcc/BM/Trace.lhs \
					$(SRCDIR2)/Control/Tracer.lhs \

TSTDIR1 = ../tbco-monitoring/test
TSTDIR2 = ../contra-tracer/test
TESTS = $(TSTDIR1)/Test.lhs \
				$(TSTDIR1)/Bcc/BM/Test/Aggregated.lhs \
				$(TSTDIR1)/Bcc/BM/Test/Configuration.lhs \
				$(TSTDIR1)/Bcc/BM/Test/Routing.lhs \
				$(TSTDIR1)/Bcc/BM/Test/STM.lhs \
				$(TSTDIR1)/Bcc/BM/Test/Structured.lhs \
				$(TSTDIR1)/Bcc/BM/Test/Trace.lhs \
				$(TSTDIR1)/Bcc/BM/Test/Tracer.lhs \
				$(TSTDIR1)/Bcc/BM/Arbitrary/Aggregated.lhs \

EXAMPLES = ../examples/complex/Main.lhs ../examples/simple/Main.lhs

TeXs = aggregation.tex configuration.tex monitoring.tex mu-benchmarks.tex output-selection.tex requirements.tex traces.tex

ImagePdf = OverviewModules.pdf SetupProcedure.pdf ConfigurationModel.pdf ConfigEditor.pdf

.PHONY: $(DOC).pdf all clean

all: $(DOC).pdf

$(DOC).tex: $(SOURCES) $(TESTS) $(EXAMPLES) $(TeXs) references.fmt
	lhs2TeX -v -o $(DOC).tex Code.lhs

$(DOC).pdf: $(DOC).tex $(ImagePdf)
	#latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make $<
	latexmk -pdf -use-make $<

OverviewModules.pdf: OverviewModules.png
	convert $< $@

SetupProcedure.pdf: SetupProcedure.png
	convert $< $@

ConfigurationModel.pdf: ConfigurationModel.png
	convert $< $@

ConfigEditor.pdf: ConfigEditor.png
	convert $< $@

clean:
	latexmk -CA

