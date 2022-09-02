#include "phonebook.h"
#include "quicksort.h"
#include "return_codes.h"
#include <cstdio>
#include <cstdlib>
#include <cstring>

using namespace std;

bool operator<(const phonebook& a, const phonebook& b);
bool operator>(const phonebook& a, const phonebook& b);

int main(int argc, char** argv)
{
	if (argc != 3)
	{
		fprintf(stderr, "Wrong number of command line arguments! You should write: <input_file_name> <output_file_name>\n");
		return ERROR_INVALID_PARAMETER;
	}

	FILE* in = fopen(argv[1], "r");
	if (!in)
	{
		fprintf(stderr, "Cannot find the input file: %s\n", argv[1]);
		return ERROR_FILE_NOT_FOUND;
	}

	char type[11] = { 0 };
	char mode[12] = { 0 };
	size_t length = 0;
	fscanf(in, "%s %s %llu", type, mode, &length);
	bool is_int = strcmp(type, "int") == 0;
	bool is_float = strcmp(type, "float") == 0;
	bool is_descending = strcmp(mode, "descending") == 0;

	int* array_int = NULL;
	float* array_float = NULL;
	phonebook* array_phonebook = NULL;
	if (is_int)
		array_int = (int*)malloc(sizeof(int) * length);
	else if (is_float)
		array_float = (float*)malloc(sizeof(float) * length);
	else
		array_phonebook = (phonebook*)malloc(sizeof(phonebook) * length);

	if (!array_int && !array_float && !array_phonebook)
	{
		fprintf(stderr, "Not enough memory resources!\n");
		fclose(in);
		return ERROR_NOT_ENOUGH_MEMORY;
	}

	if (is_int)
		for (size_t i = 0; i < length; i++)
			fscanf(in, "%i", &array_int[i]);
	else if (is_float)
		for (size_t i = 0; i < length; i++)
			fscanf(in, "%f", &array_float[i]);
	else
		for (size_t i = 0; i < length; i++)
			fscanf(in,
				   "%s %s %s %llu",
				   &array_phonebook[i].m_surname,
				   &array_phonebook[i].m_name,
				   &array_phonebook[i].m_patronymic,
				   &array_phonebook[i].m_phone_number);
	fclose(in);

	if (is_int)
	{
		if (is_descending)
			quicksort< int, true >(array_int, 0, length);
		else
			quicksort< int, false >(array_int, 0, length);
	}
	else if (is_float)
	{
		if (is_descending)
			quicksort< float, true >(array_float, 0, length);
		else
			quicksort< float, false >(array_float, 0, length);
	}
	else
	{
		if (is_descending)
			quicksort< phonebook, true >(array_phonebook, 0, length);
		else
			quicksort< phonebook, false >(array_phonebook, 0, length);
	}

	FILE* out = fopen(argv[2], "w");
	if (!out)
	{
		fprintf(stderr, "Cannot find the output file: %s\n", argv[2]);
		free(array_int);
		free(array_float);
		free(array_phonebook);
		return ERROR_FILE_NOT_FOUND;
	}

	if (is_int)
		for (size_t i = 0; i < length; i++)
			fprintf(in, "%i\n", array_int[i]);
	else if (is_float)
		for (size_t i = 0; i < length; i++)
			fprintf(in, "%f\n", array_float[i]);
	else
		for (size_t i = 0; i < length; i++)
			fprintf(
				in,
				"%s %s %s %llu\n",
				array_phonebook[i].m_surname,
				array_phonebook[i].m_name,
				array_phonebook[i].m_patronymic,
				array_phonebook[i].m_phone_number);

	fclose(out);
	free(array_int);
	free(array_float);
	free(array_phonebook);
	return 0;
}