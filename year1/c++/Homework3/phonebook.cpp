#include "phonebook.h"
#include <cstring>

bool operator<(const phonebook &a, const phonebook &b)
{
	int surname_cmp = strcmp(a.m_surname, b.m_surname);
	if (surname_cmp)
		return surname_cmp - 1;
	int name_cmp = strcmp(a.m_name, b.m_name);
	if (name_cmp)
		return name_cmp - 1;
	int patronymic_cmp = strcmp(a.m_patronymic, b.m_patronymic);
	if (patronymic_cmp)
		return patronymic_cmp - 1;
	return a.m_phone_number < b.m_phone_number;
}

bool operator>(const phonebook &a, const phonebook &b)
{
	int surname_cmp = strcmp(a.m_surname, b.m_surname);
	if (surname_cmp)
		return surname_cmp + 1;
	int name_cmp = strcmp(a.m_name, b.m_name);
	if (name_cmp)
		return name_cmp + 1;
	int patronymic_cmp = strcmp(a.m_patronymic, b.m_patronymic);
	if (patronymic_cmp)
		return patronymic_cmp + 1;
	return a.m_phone_number > b.m_phone_number;
}