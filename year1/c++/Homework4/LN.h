#pragma once

#include <string_view>

#include <cstdio>
#include <cstdlib>
#include <cstring>

class LN
{
	size_t m_length;
	size_t m_buf_size;
	char m_sign;
	char* m_digits;

  public:
	LN(long long number = 0);
	LN(const char* number);
	LN(std::string_view number);
	LN(const LN& number);
	LN(LN&& number);
	~LN() { free(m_digits); }
	LN& operator=(const LN& number);
	LN& operator=(LN&& number);
	LN operator+(const LN& number) const;
	LN operator-(const LN& number) const;
	LN operator*(const LN& number) const;
	LN operator/(const LN& number) const;
	LN operator%(const LN& number) const;
	LN operator~() const;
	LN operator-() const;
	LN& operator+=(const LN& number);
	LN& operator-=(const LN& number);
	LN& operator*=(const LN& number);
	LN& operator/=(const LN& number);
	LN& operator%=(const LN& number);
	bool operator<(const LN& number) const;
	bool operator<=(const LN& number) const;
	bool operator>(const LN& number) const;
	bool operator>=(const LN& number) const;
	bool operator==(const LN& number) const;
	bool operator!=(const LN& number) const;
	explicit operator long long() const;
	explicit operator bool() const;
	void print(FILE* stream) const;

  private:
	int compareTo(const LN& number) const;
	LN absAdd(const LN& number) const;
	LN absSubtract(const LN& number) const;
	void shiftRight();
	LN multiplyByDigit(char d) const;
	int compareByDigits(const LN& number) const;
	void removeZeroes();
	bool isNaN() const;
	void ensureCapacity(size_t requiredSize);
	void reduceCapacity();
};