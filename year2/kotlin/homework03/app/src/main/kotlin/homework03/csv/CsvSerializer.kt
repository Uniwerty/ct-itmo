package homework03

import kotlin.reflect.KClass
import kotlin.reflect.full.memberProperties

fun <T : Any> csvSerialize(data: Iterable<T>, klass: KClass<T>) = buildString { serializeObject(data, klass) }
fun <T : Any> csvSerializeHeader(klass: KClass<T>) = buildString {
    serializeHeader(klass)
    append("\n")
}

private fun <T : Any> StringBuilder.serializeObject(data: Iterable<T>, klass: KClass<T>) {
    if (data.any {
            it.javaClass.kotlin != klass
        }) throw IllegalArgumentException("not all types match")

    data.forEach {
        serializeObject(it)
        append("\n")
    }
}

private fun StringBuilder.serializeNumber(value: Number) = apply {
    append(value)
}

private fun StringBuilder.serializeValue(value: Any?) = apply {
    val kClass = value?.javaClass?.kotlin
    when (kClass) {
        String::class -> {
            serializeString(value as String)
        }
        Integer::class, Short::class, Long::class, Byte::class, Float::class, Double::class -> {
            serializeNumber(value as Number)
        }
        null -> {
            serializeString("null")
        }
    }
}

private fun StringBuilder.serializeString(value: String) = apply {
    append('"')
    append(value)
    append('"')
}

private fun <T : Any> StringBuilder.serializeHeader(klass: KClass<T>) = apply {
    val properties = klass.memberProperties

    when (klass) {
        String::class -> {
            serializeString("value")
        }
        else -> {
            properties.joinTo(this, ",") { p ->
                serializeString(p.name)
                ""
            }
        }
    }
}

private fun StringBuilder.serializeObject(value: Any) {
    val kClass = value.javaClass.kotlin
    val properties = kClass.memberProperties

    when (kClass) {
        String::class -> {
            serializeString(value as String)
        }
        else -> {
            properties.joinTo(this, ",") { p ->
                serializeValue(p.get(value) ?: null)
                ""
            }
        }
    }
}