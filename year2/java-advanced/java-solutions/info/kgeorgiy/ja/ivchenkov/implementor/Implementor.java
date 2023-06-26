package info.kgeorgiy.ja.ivchenkov.implementor;

import info.kgeorgiy.java.advanced.implementor.Impler;
import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.*;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;
import java.util.jar.JarOutputStream;
import java.util.zip.ZipEntry;

/**
 * The class for implementing interfaces.
 *
 * @author Ivchenkov Dmitrii
 */
public class Implementor implements Impler, JarImpler {
    /**
     * The {@link SimpleFileVisitor} for cleaning temporary directories while creating <var>.jar</var> files.
     */
    private static final FileVisitor<Path> TEMPORARY_DIRECTORY_CLEANER = new SimpleFileVisitor<>() {
        /**
         * Deletes specified file.
         * @param file specified file {@link Path}.
         * @param attrs file attributes.
         * @return {@link FileVisitResult result} of deleting the specified file.
         * @throws IOException if an IO error occurred during deleting the file.
         */
        @Override
        public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
            Files.delete(file);
            return FileVisitResult.CONTINUE;
        }

        /**
         * Deletes the specified directory after visiting it.
         * @param directory specified directory {@link Path}.
         * @param exception {@link IOException} occurred during the visiting, or {@code null} if the visiting succeed.
         * @return {@link FileVisitResult result} of deleting the specified directory.
         * @throws IOException if an IO error occurred during deleting the directory.
         */
        @Override
        public FileVisitResult postVisitDirectory(Path directory, IOException exception) throws IOException {
            Files.delete(directory);
            return FileVisitResult.CONTINUE;
        }
    };

    /**
     * The main method of the {@link Implementor} class.
     * <p>
     * If 2 arguments provided, implements the given interface {@code args[0]}
     * and creates the implementation class <var>.java</var> file in the path {@code args[1]}.
     * <p>
     * If 3 arguments provided and {@code args[0]} is "-jar", implements the given interface {@code args[1]}
     * and creates <var>.jar</var> file with the implementation class <var>.class</var> file in the path {@code args[2]}.
     *
     * @param args command line arguments {@link String} array.
     */
    public static void main(String[] args) {
        if (args.length != 2 && args.length != 3) {
            System.err.println("Invalid arguments number: please write");
            System.err.println("<className> <fileName> for implementation");
            System.err.println("-jar <className> <fileName> for jar-implementation");
            return;
        }
        for (String argument : args) {
            if (argument == null) {
                System.err.println("Invalid arguments: please write non-null strings!");
                return;
            }
        }
        try {
            if (args.length == 2) {
                new Implementor().implement(Class.forName(args[0]), Path.of(args[1]));
            } else if (args[0].equals("-jar")) {
                new Implementor().implementJar(Class.forName(args[1]), Path.of(args[2]));
            } else {
                System.err.println("Please write 2 or 3 correct arguments!");
            }
        } catch (ImplerException e) {
            System.err.println("Implementation error: " + e.getMessage());
        } catch (ClassNotFoundException e) {
            System.err.println("The specified class cannot be found: " + e.getMessage());
        } catch (InvalidPathException e) {
            System.err.println("Invalid path given: " + e.getMessage());
        }
    }

    /**
     * Implements the given interface {@code token} and creates <var>.java</var> file in {@code root} directory.
     *
     * @param token {@link Class type token} to create implementation for.
     * @param root  root directory {@link Path} of the created file.
     * @throws ImplerException if {@code token} is not an interface, or it is private, or
     *                         an error occurred with {@code root} path.
     */
    @Override
    public void implement(Class<?> token, Path root) throws ImplerException {
        if (!token.isInterface()) {
            throw new ImplerException("Cannot implement " + token.getCanonicalName() + " because it is not an interface!");
        } else if (Modifier.isPrivate(token.getModifiers())) {
            throw new ImplerException("Cannot implement " + token.getCanonicalName() + " because it is private!");
        }

        final Path classPath = getClassFilePath(token, root);

        try (BufferedWriter writer = Files.newBufferedWriter(classPath, StandardCharsets.UTF_8)) {
            writer.write(token.getPackage() + ";");
            writer.newLine();
            writer.write(getClassDeclarationString(token) + " {");
            writer.newLine();
            for (String memberString : getMethodDeclarationStrings(token)) {
                writer.write(memberString);
                writer.newLine();
            }
            writer.write("}");
        } catch (IOException e) {
            System.err.println("IO error occurred while writing file " + classPath + " : " + e.getMessage());
        }
    }

    /**
     * Implements the given interface {@code token} and creates <var>.jar</var> file
     * with <var>.class</var> file of the implementation class in {@code root} directory.
     * <p>
     * Creates a temporary directory for <var>.java</var> and <var>.class</var> files
     * and then deletes it.
     *
     * @param token   {@link Class type token} to create implementation for.
     * @param jarFile target <var>.jar</var> file {@link Path}.
     * @throws ImplerException if an error occurred with {@code jarFile} path, or
     *                         if an implementation error occurred, or
     *                         if a compiling error occurred, or
     *                         if a <var>.jar</var> file creation error occurred.
     */
    @Override
    public void implementJar(Class<?> token, Path jarFile) throws ImplerException {
        try {
            final Path jarRoot = jarFile.getParent();
            Files.createDirectories(jarRoot);
            final Path temporaryDir = Files.createTempDirectory(jarRoot, "tmpJarImplementor");
            implement(token, temporaryDir);
            compileFile(token, temporaryDir);
            createJar(token, jarFile, temporaryDir);
            Files.walkFileTree(temporaryDir, TEMPORARY_DIRECTORY_CLEANER);
        } catch (IOException e) {
            throw new ImplerException("IO error occurred while creating directories: " + e.getMessage());
        }
    }

    /**
     * Compiles a file of the created {@code token} implementation class in {@code root} directory.
     *
     * @param token {@link Class type token} of the interface that is being implemented.
     * @param root  root directory {@link Path} of the created file.
     * @throws ImplerException if a java compiler could not be found, or
     *                         if a compiler error occurred during compiling.
     */
    private static void compileFile(Class<?> token, final Path root) throws ImplerException {
        final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        if (compiler == null) {
            throw new ImplerException("Could not find java compiler!");
        }
        final String classpath = root + File.pathSeparator + getClassCodeSourcePath(token);
        final String[] args = new String[]{"-cp", classpath, "-encoding", "UTF-8", getClassFilePath(token, root).toString()};
        final int exitCode = compiler.run(null, null, null, args);
        if (exitCode != 0) {
            throw new ImplerException("Compiler error occurred while compiling class!");
        }
    }

    /**
     * Gets code source path string of the interface {@code token}.
     *
     * @param token {@link Class type token} of the interface that is being implemented.
     * @return source code path {@link String} of the interface {@code token}.
     * @throws ImplerException if the source code of the {@code token} interface could not be got.
     */
    private static String getClassCodeSourcePath(Class<?> token) throws ImplerException {
        try {
            return Path.of(token.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
        } catch (final URISyntaxException e) {
            throw new ImplerException("Could not get source code: " + e.getMessage());
        }
    }

    /**
     * Creates <var>.jar</var> file with the implementation of the interface {@code token} in {@code jarFile} path.
     *
     * @param token   {@link Class type token} of the interface that is being implemented.
     * @param jarFile target <var>.jar</var> file {@link Path}.
     * @param jarRoot target <var>.jar</var> file root directory {@link Path}.
     * @throws ImplerException if an IO error occurred during creating <var>.jar</var> file.
     */
    private void createJar(Class<?> token, Path jarFile, Path jarRoot) throws ImplerException {
        try (JarOutputStream outputStream = new JarOutputStream(new BufferedOutputStream(Files.newOutputStream(jarFile)))) {
            outputStream.putNextEntry(new ZipEntry(getClassFileName(token, "/", ".class")));
            Files.copy(jarRoot.resolve(getClassFileName(token, File.separator, ".class")), outputStream);
        } catch (IOException e) {
            throw new ImplerException("IO error occurred while creating jar file: " + e.getMessage());
        }
    }

    /**
     * Gets the file path of the {@code token} interface implementation class.
     *
     * @param token {@link Class type token} of the interface that is being implemented.
     * @param root  root directory {@link Path} of the file.
     * @return the implementation class file {@link Path}.
     * @throws ImplerException if an invalid path given, or
     *                         if an IO error occurred during working with files.
     */
    private static Path getClassFilePath(Class<?> token, Path root) throws ImplerException {
        try {
            final Path classPath = root.resolve(getClassFileName(token, File.separator, ".java"));
            if (classPath.getParent() != null) {
                Files.createDirectories(classPath.getParent());
            }
            return classPath;
        } catch (InvalidPathException e) {
            throw new ImplerException("Invalid path given: " + e.getMessage());
        } catch (IOException e) {
            throw new ImplerException("IO error occurred while working with files: " + e.getMessage());
        }
    }

    /**
     * Gets the file name of the {@code token} interface implementation class.
     *
     * @param token         {@link Class type token} of the interface that is being implemented.
     * @param separator     required file separator {@link String}.
     * @param fileExtension required file extension {@link String}.
     * @return {@code token} interface implementation class file name.
     */
    private static String getClassFileName(Class<?> token, String separator, String fileExtension) {
        return getImplClassName(token).replace(".", separator) + fileExtension;
    }

    /**
     * Gets the canonical name of the {@code token} interface implementation class.
     *
     * @param token {@link Class type token} of the interface that is being implemented.
     * @return the canonical name {@link String} of the {@code token} interface implementation class.
     */
    private static String getImplClassName(Class<?> token) {
        return String.format("%s.%sImpl", token.getPackageName(), token.getSimpleName());
    }

    /**
     * Gets the class declaration string of the {@code token} implementation class.
     *
     * @param token {@link Class type token} of the interface that is being implemented.
     * @return the {@code token} implementation class declaration {@link String}.
     */
    private String getClassDeclarationString(Class<?> token) {
        return String.format("public class %sImpl implements %s", token.getSimpleName(), token.getCanonicalName());
    }

    /**
     * Gets the method declaration strings of the {@code token} implementation class.
     *
     * @param token {@link Class type token} of the interface that is being implemented.
     * @return {@link List} of the method declaration {@link String strings} of the {@code token} implementation class.
     */
    private List<String> getMethodDeclarationStrings(Class<?> token) {
        return Arrays.stream(token.getMethods())
                .filter(method -> !Modifier.isStatic(method.getModifiers()))
                .map(this::getMethodDefinition)
                .toList();
    }

    /**
     * Gets the {@code method} definition string.
     *
     * @param method specified {@link Method}.
     * @return specified {@link Method} definition {@link String}.
     */
    private String getMethodDefinition(Method method) {
        return String.format("\tpublic %s %s (%s) {%n\t\treturn %s;%n\t}",
                method.getReturnType().getCanonicalName(),
                method.getName(),
                getMethodArgumentsString(method),
                getDefaultReturnValue(method.getReturnType()));
    }

    /**
     * Gets the {@code method} arguments string.
     *
     * @param method specified {@link Method}.
     * @return arguments {@link String} of the specified {@link Method}.
     */
    private String getMethodArgumentsString(Method method) {
        Class<?>[] types = method.getParameterTypes();
        StringBuilder stringBuilder = new StringBuilder();
        for (int i = 0; i < types.length; i++) {
            stringBuilder.append(types[i].getCanonicalName()).append(" arg").append(i).append(", ");
        }
        return stringBuilder.isEmpty() ? "" : stringBuilder.substring(0, stringBuilder.length() - 2);
    }

    /**
     * Gets the default return value of {@code token} class.
     *
     * @param token specified {@link Class}.
     * @return default return value {@link String} of the specified class.
     */
    private String getDefaultReturnValue(Class<?> token) {
        if (token == void.class) {
            return "";
        } else if (token == boolean.class) {
            return "false";
        } else if (token.isPrimitive()) {
            return "0";
        } else {
            return "null";
        }
    }
}