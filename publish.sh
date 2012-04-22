SCALATRON=/home/jserrin/projects/scalatron/scalatron-0.9.3/Scalatron
DEST=$SCALATRON/bots/JoshSerrin
mkdir $DEST

sbt package

cp target/scala-2.9.1/cra_scalatron-jls*.jar $DEST/ScalatronBot.jar
