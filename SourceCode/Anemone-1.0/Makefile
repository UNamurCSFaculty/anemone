compile :
	sbt compile

assembly :
	sbt compile
	sbt package
	sbt assembly

run :
	scala -classpath lib/processing.core.jar:lib/scala-parser-combinators_2.12-1.0.7.jar:lib/scala-swing_2.12-2.1.1.jar target/scala-2.12/root_2.12-0.1.0-SNAPSHOT.jar 

run-java :
	java -jar target/scala-2.12/root-assembly-0.1.0-SNAPSHOT.jar
