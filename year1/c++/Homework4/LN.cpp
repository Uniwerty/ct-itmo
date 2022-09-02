#include "LN.h"

LN::LN(long long number)
{
	if (number == 0)
	{
		m_sign = 1;
		m_length = 1;
		m_buf_size = 1;
		m_digits = (char *)malloc(1);
		if (!m_digits)
		{
			throw "Not enough memory recourses!";
		}
		m_digits[0] = 0;
	}
	else
	{
		m_sign = number < 0 ? -1 : 1;
		m_length = 0;
		long long n = number;
		while (n)
		{
			m_length++;
			n /= 10;
		}
		m_digits = (char *)malloc(m_length);
		m_buf_size = m_length;
		if (!m_digits)
		{
			throw "Not enough memory recourses!";
		}
		size_t pos = 0;
		number = std::abs(number);
		while (number)
		{
			m_digits[pos] = number % 10;
			number /= 10;
			pos++;
		}
	}
}
LN::LN(const char *number)
{
	if (strcmp(number, "NaN") == 0)
	{
		m_sign = 0;
		m_length = 0;
		m_buf_size = 0;
		m_digits = nullptr;
		return;
	}
	m_sign = number[0] == '-' ? -1 : 1;
	size_t len = strlen(number);
	size_t pos = 0;
	if (number[0] == '-')
		pos = 1;
	while (pos < len && number[pos] == '0')
		pos++;
	if (pos == len)
	{
		m_sign = 1;
		m_length = 1;
		m_buf_size = 1;
		m_digits = (char *)malloc(1);
		if (!m_digits)
		{
			throw "Not enough memory recourses!";
		}
		m_digits[0] = 0;
	}
	else
	{
		m_length = len - pos;
		m_digits = (char *)malloc(m_length);
		m_buf_size = m_length;
		if (!m_digits)
		{
			throw "Not enough memory recourses!";
		}
		for (size_t i = pos; i < len; i++)
			m_digits[len - i - 1] = number[i] - '0';
	}
}
LN::LN(std::string_view number)
{
	if (number == "NaN")
	{
		m_sign = 0;
		m_length = 0;
		m_buf_size = 0;
		m_digits = nullptr;
		return;
	}
	m_sign = number[0] == '-' ? -1 : 1;
	size_t pos = 0;
	if (number[0] == '-')
		pos = 1;
	while (pos < number.length() && number[pos] == '0')
		pos++;
	if (pos == number.length())
	{
		m_sign = 1;
		m_length = 1;
		m_buf_size = 1;
		m_digits = (char *)malloc(1);
		if (!m_digits)
		{
			throw "Not enough memory recourses!";
		}
		m_digits[0] = 0;
	}
	else
	{
		m_length = number.length() - pos;
		m_digits = (char *)malloc(m_length);
		m_buf_size = m_length;
		if (!m_digits)
		{
			throw "Not enough memory recourses!";
		}
		for (size_t i = pos; i < number.length(); i++)
		{
			m_digits[number.length() - i - 1] = number[i] - '0';
		}
	}
}
LN::LN(const LN &number)
{
	m_sign = number.m_sign;
	m_length = number.m_length;
	m_buf_size = number.m_buf_size;
	m_digits = (char *)malloc(m_buf_size);
	if (!m_digits)
	{
		throw "Not enough memory recourses!";
	}
	for (size_t i = 0; i < m_length; i++)
		m_digits[i] = number.m_digits[i];
}
LN::LN(LN &&number)
{
	m_sign = number.m_sign;
	m_length = number.m_length;
	m_buf_size = number.m_buf_size;
	m_digits = number.m_digits;
	number.m_digits = nullptr;
}

// Assignment operators

LN &LN::operator=(const LN &number)
{
	if (this != &number)
	{
		m_sign = number.m_sign;
		m_length = number.m_length;
		if (m_buf_size != number.m_buf_size)
		{
			free(m_digits);
			m_buf_size = number.m_buf_size;
			m_digits = (char *)malloc(m_buf_size);
			if (!m_digits)
			{
				throw "Not enough memory recourses!";
			}
		}
		for (size_t i = 0; i < m_length; i++)
			m_digits[i] = number.m_digits[i];
	}
	return *this;
}
LN &LN::operator=(LN &&number)
{
	if (this != &number)
	{
		free(m_digits);
		m_sign = number.m_sign;
		m_length = number.m_length;
		m_buf_size = number.m_buf_size;
		m_digits = number.m_digits;
		number.m_digits = nullptr;
	}
	return *this;
}

// Arithmetic operators

LN LN::operator+(const LN &number) const
{
	if (isNaN() || number.isNaN())
		return { "NaN" };
	LN result;
	if (m_sign == -1 && number.m_sign == -1)
		result = -absAdd(number);
	else if (m_sign == 1 && number.m_sign == -1)
		result = absSubtract(-number);
	else if (m_sign == -1 && number.m_sign == 1)
		result = number.absSubtract(-(*this));
	else
		result = absAdd(number);
	result.removeZeroes();
	return result;
}
LN LN::operator-(const LN &number) const
{
	if (isNaN() || number.isNaN())
		return { "NaN" };
	LN result;
	if (m_sign == -1 && number.m_sign == -1)
		result = (-number).absSubtract(-(*this));
	else if (m_sign == 1 && number.m_sign == -1)
		result = absAdd(-number);
	else if (m_sign == -1 && number.m_sign == 1)
		result = -number.absAdd(-(*this));
	else
		result = absSubtract(number);
	result.removeZeroes();
	return result;
}
LN LN::operator*(const LN &number) const
{
	if (isNaN() || number.isNaN())
		return { "NaN" };
	if (!*this || !number)
		return { 0ll };
	LN result;
	result.m_sign = m_sign * number.m_sign;
	result.m_length = m_length + number.m_length;
	result.ensureCapacity(result.m_length);
	for (size_t i = 0; i < result.m_length; i++)
		result.m_digits[i] = 0;
	int c = 0;
	for (size_t i = 0; i < m_length; i++)
	{
		for (size_t j = 0; j < number.m_length || c; j++)
		{
			c += result.m_digits[i + j] + m_digits[i] * (j < number.m_length ? number.m_digits[j] : 0);
			result.m_digits[i + j] = c % 10;
			c /= 10;
		}
	}
	result.removeZeroes();
	return result;
}
LN LN::operator/(const LN &number) const
{
	if (isNaN() || number.isNaN() || !number)
		return { "NaN" };
	if (!*this || compareByDigits(number) < 0)
		return { 0ll };
	LN result = *this;
	result.m_sign = m_sign * number.m_sign;
	LN cur = *this;
	cur.m_sign = 1;
	for (size_t i = 0; i < cur.m_length; i++)
	{
		cur.m_digits[i] = 0;
		result.m_digits[i] = 0;
	}
	for (size_t i = m_length; i--;)
	{
		cur.shiftRight();
		cur.m_digits[0] = m_digits[i];
		char l = 0, r = 10, d = 0;
		while (r - l > 1)
		{
			char m = (l + r) >> 1;
			if (number.multiplyByDigit(m).compareByDigits(cur) <= 0)
			{
				d = m;
				l = m;
			}
			else
				r = m;
		}
		result.m_digits[i] = d;
		cur = cur.absSubtract(number.multiplyByDigit(d));
	}
	result.removeZeroes();
	return result;
}
LN LN::operator%(const LN &number) const
{
	if (isNaN() || number.isNaN() || !number)
		return { "NaN" };
	return *this - *this / number * number;
}
LN LN::operator~() const
{
	if (isNaN() || m_sign == -1)
		return { "NaN" };
	LN result = *this;
	LN cur = *this;
	cur.m_sign = 1;
	size_t length = m_length;
	for (size_t i = 0; i < m_length; i++)
	{
		result.m_digits[i] = 0;
		cur.m_digits[i] = 0;
	}
	if (m_length % 2)
	{
		if (m_digits[length - 1] == 9)
			result.m_digits[0] = 3;
		else if (m_digits[length - 1] >= 4)
			result.m_digits[0] = 2;
		else if (m_digits[length - 1] >= 1)
			result.m_digits[0] = 1;
		else
			result.m_digits[0] = 0;
		cur.m_digits[0] = m_digits[length - 1] - result.m_digits[0] * result.m_digits[0];
		length--;
	}
	else
	{
		char l = 0, r = 10, d = 0, x = m_digits[length - 1] * 10 + m_digits[length - 2];
		while (r - l > 1)
		{
			char m = (l + r) >> 1;
			if (m * m <= x)
			{
				d = m;
				l = m;
			}
			else
				r = m;
		}
		result.m_digits[0] = d;
		cur.m_digits[0] = (x - d * d) % 10;
		cur.m_digits[1] = (x - d * d) / 10;
		length -= 2;
	}
	for (; length--;)
	{
		result.shiftRight();
		cur.shiftRight();
		cur.shiftRight();
		cur.m_digits[1] = m_digits[length];
		cur.m_digits[0] = m_digits[length - 1];
		length--;
		LN a = result.multiplyByDigit(2);
		char l = 0, r = 10, d = 0;
		while (r - l > 1)
		{
			char m = (l + r) >> 1;
			a.m_digits[0] = m;
			if (a.multiplyByDigit(m).compareByDigits(cur) <= 0)
			{
				d = m;
				l = m;
			}
			else
				r = m;
		}
		result.m_digits[0] = d;
		a.m_digits[0] = d;
		cur = cur.absSubtract(a.multiplyByDigit(d));
	}
	result.removeZeroes();
	return result;
}
LN LN::operator-() const
{
	if (isNaN())
		return { "NaN" };
	if (!(*this))
		return { 0ll };
	LN number = *this;
	number.m_sign = -m_sign;
	return number;
}
LN &LN::operator+=(const LN &number)
{
	*this = *this + number;
	return *this;
}
LN &LN::operator-=(const LN &number)
{
	*this = *this - number;
	return *this;
}
LN &LN::operator*=(const LN &number)
{
	*this = *this * number;
	return *this;
}
LN &LN::operator/=(const LN &number)
{
	*this = *this / number;
	return *this;
}
LN &LN::operator%=(const LN &number)
{
	*this = *this % number;
	return *this;
}

// Comparison operators

bool LN::operator<(const LN &number) const
{
	if (isNaN() || number.isNaN())
		return false;
	return compareTo(number) < 0;
}
bool LN::operator<=(const LN &number) const
{
	if (isNaN() || number.isNaN())
		return false;
	return compareTo(number) <= 0;
}
bool LN::operator>(const LN &number) const
{
	if (isNaN() || number.isNaN())
		return false;
	return compareTo(number) > 0;
}
bool LN::operator>=(const LN &number) const
{
	if (isNaN() || number.isNaN())
		return false;
	return compareTo(number) >= 0;
}
bool LN::operator==(const LN &number) const
{
	if (isNaN() || number.isNaN())
		return false;
	return compareTo(number) == 0;
}
bool LN::operator!=(const LN &number) const
{
	if (isNaN() || number.isNaN())
		return true;
	return compareTo(number) != 0;
}

// Cast operators

LN::operator long long() const
{
	if (isNaN())
	{
		throw "Cannot cast to long long: the number is NaN!";
	}
	if (m_length > 19)
	{
		throw "Cannot cast to long long: the number is too large!";
	}
	long long number = 0;
	long long factor = m_sign;
	for (size_t i = 0; i < m_length; i++)
	{
		number += factor * m_digits[i];
		factor *= 10;
	}
	if (m_sign < 0 && number > 0 || m_sign > 0 && number < 0)
	{
		throw "Cannot cast to long long: the number is too large!";
	}
	return number;
}
LN::operator bool() const
{
	if (m_sign == 1 && m_length == 1 && m_digits[0] == 0)
		return false;
	return true;
}
LN operator""_ln(const char *number)
{
	return { number };
}

void LN::print(FILE *stream) const
{
	if (isNaN())
	{
		fprintf(stream, "NaN");
		return;
	}
	if (m_sign == -1)
		fprintf(stream, "-");
	for (size_t i = m_length; i--;)
		fprintf(stream, "%u", m_digits[i]);
}

// Private methods

int LN::compareTo(const LN &number) const
{
	if (m_sign == -1 && number.m_sign == 1 || m_sign == 1 && number.m_sign == 1 && m_length < number.m_length ||
		m_sign == -1 && number.m_sign == -1 && m_length > number.m_length)
		return -1;
	if (m_sign == 1 && number.m_sign == -1 || m_sign == 1 && number.m_sign == 1 && m_length > number.m_length ||
		m_sign == -1 && number.m_sign == -1 && m_length < number.m_length)
		return 1;
	if (m_sign == number.m_sign && m_length == number.m_length)
	{
		for (size_t i = m_length; i--;)
		{
			if (m_digits[i] < number.m_digits[i])
				return -m_sign;
			if (m_digits[i] > number.m_digits[i])
				return m_sign;
		}
	}
	return 0;
}
LN LN::absAdd(const LN &number) const
{
	LN result;
	result.m_length = m_length > number.m_length ? m_length : number.m_length;
	result.ensureCapacity(result.m_length);
	unsigned char c = 0;
	for (size_t i = 0; i < result.m_length; i++)
	{
		c += (i < m_length ? m_digits[i] : 0) + (i < number.m_length ? number.m_digits[i] : 0);
		if (c > 9)
		{
			result.m_digits[i] = c - 10;
			c = 1;
		}
		else
		{
			result.m_digits[i] = c;
			c = 0;
		}
	}
	if (c > 0)
	{
		result.m_length++;
		result.ensureCapacity(result.m_length);
		result.m_digits[result.m_length - 1] = c;
	}
	return result;
}
LN LN::absSubtract(const LN &number) const
{
	if (*this < number)
		return -number.absSubtract(*this);
	LN result = *this;
	result.m_sign = 1;
	for (size_t i = 0; i < result.m_length; i++)
	{
		if (result.m_digits[i] < 0)
		{
			result.m_digits[i + 1]--;
			result.m_digits[i] += 10;
		}
		result.m_digits[i] -= i < number.m_length ? number.m_digits[i] : 0;
		if (result.m_digits[i] < 0)
		{
			result.m_digits[i + 1]--;
			result.m_digits[i] += 10;
		}
	}
	return result;
}
void LN::shiftRight()
{
	for (size_t i = m_length - 1; i >= 1; i--)
		m_digits[i] = m_digits[i - 1];
	m_digits[0] = 0;
}
LN LN::multiplyByDigit(const char d) const
{
	LN result = *this;
	char c = 0;
	for (size_t i = 0; i < m_length; i++)
	{
		c += m_digits[i] * d;
		result.m_digits[i] = c % 10;
		c /= 10;
	}
	if (c > 0)
	{
		result.m_length++;
		result.ensureCapacity(result.m_length);
		result.m_digits[result.m_length - 1] = c;
	}
	return result;
}
int LN::compareByDigits(const LN &number) const
{
	for (size_t i = std::max(m_length, number.m_length); i--;)
	{
		char d1 = i < m_length ? m_digits[i] : 0;
		char d2 = i < number.m_length ? number.m_digits[i] : 0;
		if (d1 < d2)
			return -1;
		if (d1 > d2)
			return 1;
	}
	return 0;
}
void LN::removeZeroes()
{
	size_t i = m_length - 1;
	while (i > 0 && m_digits[i] == 0)
		i--;
	m_length = i + 1;
	reduceCapacity();
}
bool LN::isNaN() const
{
	return m_sign == 0 && m_length == 0;
}
void LN::ensureCapacity(const size_t requiredSize)
{
	size_t new_buf_size = m_buf_size;
	while (new_buf_size < requiredSize)
		new_buf_size *= 2;
	char *new_buffer = (char *)realloc(m_digits, new_buf_size);
	if (!new_buffer)
	{
		new_buf_size = requiredSize + 1;
		new_buffer = (char *)realloc(m_digits, new_buf_size);
		if (!new_buffer)
		{
			throw "Not enough memory resources!";
		}
	}
	else
	{
		m_buf_size = new_buf_size;
		m_digits = new_buffer;
	}
}
void LN::reduceCapacity()
{
	if (m_length > 0 && m_length * 4 < m_buf_size)
	{
		m_buf_size = m_length * 2;
		m_digits = (char *)realloc(m_digits, m_buf_size);
		if (!m_digits)
		{
			throw "Not enough memory resources!";
		}
	}
}