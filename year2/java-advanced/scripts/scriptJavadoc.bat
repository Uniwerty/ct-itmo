@ECHO OFF
set path1=../../java-advanced-2023/modules/info.kgeorgiy.java.advanced.implementor/info/kgeorgiy/java/advanced/implementor
set path2=../java-solutions/info/kgeorgiy/ja/ivchenkov/implementor
javadoc -d ../javadoc -private -author %path1%/ImplerException.java %path1%/Impler.java %path1%/JarImpler.java %path2%/Implementor.java
