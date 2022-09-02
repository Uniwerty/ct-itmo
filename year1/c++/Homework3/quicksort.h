#pragma once
#include <cstdio>

template< typename T, bool descending >
void quicksort(T* array, size_t left, size_t right)
{
	while (right - left > 1)
	{
		T x = array[(left + right) / 2];
		size_t l0 = 0, l1 = 0, l2 = 0;
		for (size_t i = left; i < right; i++)
		{
			if (!descending && array[i] > x || descending && array[i] < x)
			{
				l2++;
				continue;
			}
			if ((!descending && array[i] < x || descending && array[i] > x) && l1 >= 1)
			{
				T tmp = array[i];
				array[i] = array[left + l0];
				array[left + l0] = tmp;
				l0++;
				l1--;
			}
			T tmp = array[i];
			array[i] = array[left + l0 + l1];
			array[left + l0 + l1] = tmp;
			if (!descending && array[left + l0 + l1] < x || descending && array[left + l0 + l1] > x)
				l0++;
			else
				l1++;
		}
		if (l0 < l2)
		{
			quicksort< T, descending >(array, left, left + l0);
			left = left + l0 + l1;
		}
		else
		{
			quicksort< T, descending >(array, left + l0 + l1, right);
			right = left + l0;
		}
	}
}