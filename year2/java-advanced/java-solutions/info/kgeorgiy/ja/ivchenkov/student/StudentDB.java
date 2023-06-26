package info.kgeorgiy.ja.ivchenkov.student;

import info.kgeorgiy.java.advanced.student.GroupName;
import info.kgeorgiy.java.advanced.student.Student;
import info.kgeorgiy.java.advanced.student.StudentQuery;

import java.util.*;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class StudentDB implements StudentQuery {
    private final static Comparator<? super Student> STUDENT_NAME_COMPARATOR =
            Comparator.comparing(Student::getLastName)
                    .thenComparing(Student::getFirstName)
                    .reversed()
                    .thenComparingInt(Student::getId);
    private final static Collector<Student, ?, Map<String, String>> STUDENT_NAME_MAP_COLLECTOR =
            Collectors.toMap(
                    Student::getLastName,
                    Student::getFirstName,
                    BinaryOperator.minBy(Comparable::compareTo)
            );

    @Override
    public List<String> getFirstNames(List<Student> students) {
        return getValuesList(students, Student::getFirstName);
    }

    @Override
    public List<String> getLastNames(List<Student> students) {
        return getValuesList(students, Student::getLastName);
    }

    @Override
    public List<GroupName> getGroups(List<Student> students) {
        return getValuesList(students, Student::getGroup);
    }

    @Override
    public List<String> getFullNames(List<Student> students) {
        return getValuesList(students, student -> student.getFirstName() + " " + student.getLastName());
    }

    @Override
    public Set<String> getDistinctFirstNames(List<Student> students) {
        return getValuesSet(students, Student::getFirstName);
    }

    @Override
    public String getMaxStudentFirstName(List<Student> students) {
        return getMaxStudentValueBy(students, Student::getFirstName, "");
    }

    @Override
    public List<Student> sortStudentsById(Collection<Student> students) {
        return sortBy(students, Comparator.naturalOrder());
    }

    @Override
    public List<Student> sortStudentsByName(Collection<Student> students) {
        return sortBy(students, STUDENT_NAME_COMPARATOR);
    }

    @Override
    public List<Student> findStudentsByFirstName(Collection<Student> students, String name) {
        return findByValueToList(students, Student::getFirstName, name);
    }

    @Override
    public List<Student> findStudentsByLastName(Collection<Student> students, String name) {
        return findByValueToList(students, Student::getLastName, name);
    }

    @Override
    public List<Student> findStudentsByGroup(Collection<Student> students, GroupName group) {
        return findByValueToList(students, Student::getGroup, group);
    }

    @Override
    public Map<String, String> findStudentNamesByGroup(Collection<Student> students, GroupName group) {
        return findByValue(students, Student::getGroup, group).collect(STUDENT_NAME_MAP_COLLECTOR);
    }

    private <R> Stream<R> getValues(List<Student> students, Function<Student, R> function) {
        return students.stream().map(function);
    }

    private <R> List<R> getValuesList(List<Student> students, Function<Student, R> function) {
        return getValues(students, function).toList();
    }

    private <R> Set<R> getValuesSet(List<Student> students, Function<Student, R> function) {
        return getValues(students, function).collect(Collectors.toCollection(TreeSet::new));
    }

    private <R> R getMaxStudentValueBy(List<Student> students, Function<Student, R> function, R defaultValue) {
        return students.stream().max(Comparator.naturalOrder()).map(function).orElse(defaultValue);
    }

    private List<Student> sortBy(Collection<Student> students, Comparator<? super Student> comparator) {
        return students.stream().sorted(comparator).toList();
    }

    private Stream<Student> findBy(Collection<Student> students, Predicate<Student> predicate) {
        return students.stream().filter(predicate).sorted(STUDENT_NAME_COMPARATOR);
    }

    private <R> Stream<Student> findByValue(Collection<Student> students, Function<Student, R> function, R value) {
        return findBy(students, student -> Objects.equals(function.apply(student), value));
    }

    private <R> List<Student> findByValueToList(Collection<Student> students, Function<Student, R> function, R value) {
        return findByValue(students, function, value).toList();
    }
}