#include "return_codes.h"

#include <malloc.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>

// Смена местами двух строк
void swap_lines(int n, float a[n][n + 1], float eps[n], float max_value[n], int i, int j)
{
	if (i != j)
	{
		for (int k = 0; k < n + 1; k++)
		{
			float tmp = a[i][k];
			a[i][k] = a[j][k];
			a[j][k] = tmp;
		}
		float tmp = eps[i];
		eps[i] = eps[j];
		eps[j] = tmp;

		tmp = max_value[i];
		max_value[i] = max_value[j];
		max_value[j] = tmp;
	}
}

// Смена местами двух столбцов
void swap_columns(int n, float a[n][n + 1], int order[n], int i, int j)
{
	if (i != j)
	{
		for (int k = 0; k < n; k++)
		{
			float tmp = a[k][i];
			a[k][i] = a[k][j];
			a[k][j] = tmp;
		}
		order[i] = j;
		order[j] = i;
	}
}

// Проверка ненулевого элемента на диагонали
bool check_element(int n, float a[n][n + 1], int order[n], float eps[n], float max_value[n], int line)
{
	if (fabsf(a[line][line]) > eps[line])
	{
		return true;
	}
	int cur = line;
	while (line < n)
	{
		for (int i = line; i < n; i++)
		{
			if (fabsf(a[i][line]) > eps[i])
			{
				swap_lines(n, a, eps, max_value, cur, i);
				swap_columns(n, a, order, cur, line);
				return true;
			}
			if (fabsf(a[line][i]) > eps[line])
			{
				swap_columns(n, a, order, cur, i);
				swap_lines(n, a, eps, max_value, cur, line);
				return true;
			}
		}
		line++;
	}
	return false;
}

// Проверка количества решений системы
int check_solvability(int n, float a[n][n + 1], float eps[n])
{
	bool contains_null_line = false;
	for (int i = 0; i < n; i++)
	{
		bool is_null = true;
		for (int j = 0; j < n; j++)
		{
			if (fabsf(a[i][j]) > eps[i])
			{
				is_null = false;
				break;
			}
		}
		if (is_null)
		{
			if (fabsf(a[i][n]) > eps[i])
				return -1;
			else
				contains_null_line = true;
		}
	}
	if (contains_null_line)
		return 1;
	return 0;
}

// Прямой ход метода Гаусса
void forward_elimination(int n, float a[n][n + 1], float eps[n], float max_value[n], int line)
{
	float k = a[line][line];
	eps[line] /= k;
	for (int j = line; j < n + 1; j++)
		a[line][j] /= k;
	for (int i = line + 1; i < n; i++)
	{
		float m = a[i][line];
		for (int j = line; j < n + 1; j++)
		{
			a[i][j] -= a[line][j] * m;
			if (fabsf(a[i][j]) > max_value[i])
				max_value[i] = fabsf(a[i][j]);
		}
		eps[i] = max_value[i] * 0.00001f;
	}
}

// Обратный ход метода Гаусса
void back_substitution(int n, float a[n][n + 1], int line)
{
	float m = a[line][n];
	for (int i = line - 1; i >= 0; i--)
	{
		a[i][n] -= a[i][line] * m;
	}
}

// Решение системы
int solution(int n, float a[n][n + 1], int order[n], float eps[n], float max_value[n])
{
	for (int i = 0; i < n; i++)
	{
		if (!check_element(n, a, order, eps, max_value, i))
			break;
		forward_elimination(n, a, eps, max_value, i);
	}
	int solvability = check_solvability(n, a, eps);
	if (solvability)
		return solvability;
	for (int i = n - 1; i > 0; i--)
		back_substitution(n, a, i);
	return 0;
}

int main(int argc, char **argv)
{
	if (argc != 3)
	{
		printf("Wrong number of command line arguments!\nYou should write: <input_file_name> <output_file_name>");
		return ERROR_INVALID_DATA;
	}

	FILE *in = fopen(argv[1], "r");
	if (!in)
	{
		printf("Cannot find the input file: %s", argv[1]);
		return ERROR_FILE_NOT_FOUND;
	}

	int n;
	fscanf(in, "%i", &n);
	float(*a)[n + 1] = malloc(sizeof(float) * n * (n + 1));
	int *order = malloc(sizeof(int) * n);
	float *eps = malloc(sizeof(float) * n);
	float *max_value = malloc(sizeof(float) * n);

	if (a == NULL || order == NULL || eps == NULL || max_value == NULL)
	{
		printf("Not enough memory resources!");
		fclose(in);
		free(a);
		free(order);
		free(eps);
		free(max_value);
		return ERROR_NOT_ENOUGH_MEMORY;
	}

	for (int i = 0; i < n; i++)
	{
		fscanf(in, "%f", &a[i][0]);
		max_value[i] = fabsf(a[i][0]);
		for (int j = 1; j < n; j++)
		{
			fscanf(in, "%f", &a[i][j]);
			if (fabsf(a[i][j]) > max_value[i])
				max_value[i] = fabsf(a[i][j]);
		}
		fscanf(in, "%f", &a[i][n]);
		eps[i] = max_value[i] * 0.00001f;
	}
	for (int i = 0; i < n; i++)
		order[i] = i;

	fclose(in);

	FILE *out = fopen(argv[2], "w");
	if (!out)
	{
		printf("Cannot find the output file: %s", argv[2]);
		free(a);
		free(order);
		free(eps);
		free(max_value);
		return ERROR_FILE_NOT_FOUND;
	}

	int solvability = solution(n, a, order, eps, max_value);
	if (solvability == -1)
		fprintf(out, "no solution");
	else if (solvability == 1)
		fprintf(out, "many solutions");
	else
	{
		for (int i = 0; i < n; i++)
			fprintf(out, "%g\n", a[order[i]][n]);
	}
	fclose(out);
	free(a);
	free(order);
	free(eps);
	free(max_value);
	return 0;
}