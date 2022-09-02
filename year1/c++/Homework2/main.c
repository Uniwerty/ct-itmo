#if defined(LIBDEFLATE)
	#include <libdeflate.h>
#elif defined(ZLIB)
	#include <zlib.h>
#elif defined(ISAL)
	#include <include/igzip_lib.h>
#else
	#error("Invalid macro!")
#endif

#include "return_codes.h"
#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>

unsigned char paeth_predictor(unsigned char a, unsigned char b, unsigned char c)
{
	int p, pa, pb, pc;
	p = a + b - c;
	pa = abs(p - a);
	pb = abs(p - b);
	pc = abs(p - c);
	if (pa <= pb && pa <= pc)
		return a;
	else if (pb <= pc)
		return b;
	else
		return c;
}

int main(int argc, char **argv)
{
	if (argc != 3)
	{
		fprintf(stderr, "Wrong number of command line arguments!\nYou should write: <input_PNG_file_name> <output_PNM_file_name>\n");
		return ERROR_INVALID_PARAMETER;
	}

	FILE *in = fopen(argv[1], "rb");
	if (!in)
	{
		fprintf(stderr, "Cannot find the input file: %s\n", argv[1]);
		return ERROR_FILE_NOT_FOUND;
	}

	// File signature checking
	unsigned char ident[8];
	fread(ident, 1, 8, in);
	if (!(ident[0] == 0x89 && ident[1] == 0x50 && ident[2] == 0x4E && ident[3] == 0x47 && ident[4] == 0x0D &&
		  ident[5] == 0x0A && ident[6] == 0x1A && ident[7] == 0x0A))
	{
		fprintf(stderr, "Input file format is incorrect (PNG required): %s\n", argv[1]);
		fclose(in);
		return ERROR_INVALID_DATA;
	}

	// IHDR chunk parsing
	unsigned char length[4], chunk_type[4], crc[4];
	fread(&length, 1, 4, in);
	fread(chunk_type, 1, 4, in);
	unsigned char width[4], height[4];
	unsigned char bit_depth, color_type, cmpr_method, filter_method, intr_method;
	fread(&width, 4, 1, in);
	fread(&height, 4, 1, in);
	fread(&bit_depth, 1, 1, in);
	fread(&color_type, 1, 1, in);
	fread(&cmpr_method, 1, 1, in);
	fread(&filter_method, 1, 1, in);
	fread(&intr_method, 1, 1, in);
	fread(&crc, 1, 4, in);
	size_t image_width = width[0] * 0x1000000 + width[1] * 0x10000 + width[2] * 0x100 + width[3];
	size_t image_height = height[0] * 0x1000000 + height[1] * 0x1000 + height[2] * 0x100 + height[3];
	// File verification
	if (!(chunk_type[0] == 'I' && chunk_type[1] == 'H' && chunk_type[2] == 'D' && chunk_type[3] == 'R'))
	{
		fprintf(stderr, "IHDR chunk must appear first, but %c%c%c%c found. The data is invalid!\n", chunk_type[0], chunk_type[1], chunk_type[2], chunk_type[3]);
		fclose(in);
		return ERROR_INVALID_DATA;
	}
	if (image_width == 0 || image_width > (1 << 31))
	{
		fprintf(stderr, "The image width must be a non-zero value less than or equal to 2^31, but it is %u!\n", image_width);
		fclose(in);
		return ERROR_INVALID_DATA;
	}
	if (image_height == 0 || image_height > (1 << 31))
	{
		fprintf(stderr, "The image height must be a non-zero value less than or equal to 2^31, but it is %u!\n", image_height);
		fclose(in);
		return ERROR_INVALID_DATA;
	}
	if (bit_depth != 8)
	{
		fprintf(stderr, "The bit depth should be 8, not %u!\n", bit_depth);
		fclose(in);
		return ERROR_INVALID_DATA;
	}
	if (color_type != 0 && color_type != 2)
	{
		fprintf(stderr, "The color type should be 0 or 2, not %u!\n", color_type);
		fclose(in);
		return ERROR_INVALID_DATA;
	}
	if (cmpr_method != 0)
	{
		fprintf(stderr, "The compression method must be 0, not %u!\n", cmpr_method);
		fclose(in);
		return ERROR_INVALID_DATA;
	}
	if (filter_method != 0)
	{
		fprintf(stderr, "The filter method must be 0, not %u!\n", filter_method);
		fclose(in);
		return ERROR_INVALID_DATA;
	}
	if (intr_method != 0)
	{
		fprintf(stderr, "The interlace method should be 0, not %u!\n", intr_method);
		fclose(in);
		return ERROR_INVALID_DATA;
	}

	// Other chunks parsing
	unsigned char *deflate_stream = NULL;
	size_t in_buffer_size = 0;
	unsigned char c_type[4] = { 0, 0, 0, 0 };
	while (!(c_type[0] == 'I' && c_type[1] == 'E' && c_type[2] == 'N' && c_type[3] == 'D'))
	{
		unsigned char c_length[4], c_crc[4];
		fread(c_length, 1, 4, in);
		fread(c_type, 1, 4, in);
		size_t data_size = c_length[0] * 0x1000000 + c_length[1] * 0x10000 + c_length[2] * 0x100 + c_length[3];
		// Ancillary chunks skipping
		if ((c_type[0] >> 5) % 2)
			fseek(in, (long)data_size, SEEK_CUR);
		// IDAT chunks saving
		else if (c_type[0] == 'I' && c_type[1] == 'D' && c_type[2] == 'A' && c_type[3] == 'T')
		{
			unsigned char *tmp = realloc(deflate_stream, in_buffer_size + data_size);
			if (tmp)
				deflate_stream = tmp;
			else
			{
				fprintf(stderr, "Not enough memory resources!\n");
				fclose(in);
				free(deflate_stream);
				return ERROR_NOT_ENOUGH_MEMORY;
			}
			fread(deflate_stream + in_buffer_size, 1, data_size, in);
			in_buffer_size += data_size;
		}
		fread(c_crc, 1, 4, in);
	}
	fclose(in);

	// Deflate stream decompressing
	size_t out_buffer_size = in_buffer_size;
	size_t *actual_out_size = &out_buffer_size;
	unsigned char *output_data = malloc(out_buffer_size);
	if (!output_data)
	{
		fprintf(stderr, "Not enough memory resources!\n");
		free(deflate_stream);
		return ERROR_NOT_ENOUGH_MEMORY;
	}

#if defined(LIBDEFLATE)
	struct libdeflate_decompressor *decompressor = libdeflate_alloc_decompressor();
	if (!decompressor)
	{
		fprintf(stderr, "Not enough memory resources!\n");
		free(deflate_stream);
		free(output_data);
		return ERROR_NOT_ENOUGH_MEMORY;
	}
	enum libdeflate_result result =
		libdeflate_zlib_decompress(decompressor, deflate_stream, in_buffer_size, output_data, out_buffer_size, actual_out_size);
#elif defined(ZLIB)
	int result = uncompress(output_data, actual_out_size, deflate_stream, in_buffer_size);
#elif defined(ISAL)
	struct inflate_state state;
	struct inflate_state *state_ptr = &state;
	isal_inflate_init(state_ptr);
	state_ptr->next_in = deflate_stream;
	state_ptr->avail_in = in_buffer_size;
	state_ptr->next_out = output_data;
	state_ptr->avail_out = out_buffer_size;
	state_ptr->crc_flag = ISAL_ZLIB;
	int result = isal_inflate_stateless(state_ptr);
#endif

#if defined(LIBDEFLATE)
	while (result == LIBDEFLATE_INSUFFICIENT_SPACE)
#elif defined(ZLIB)
	while (result == Z_BUF_ERROR)
#elif defined(ISAL)
	while (result == ISAL_OUT_OVERFLOW)
#endif
	{
		size_t new_out_buffer_size = out_buffer_size * 2;
		unsigned char *new_out_buffer = realloc(output_data, new_out_buffer_size);
		if (!new_out_buffer)
		{
			new_out_buffer_size = out_buffer_size + out_buffer_size / 4;
			new_out_buffer = realloc(output_data, new_out_buffer_size);
			if (!new_out_buffer)
			{
				fprintf(stderr, "Not enough memory resources!\n");
				free(deflate_stream);
				free(output_data);
#if defined(LIBDEFLATE)
				libdeflate_free_decompressor(decompressor);
#endif
				return ERROR_NOT_ENOUGH_MEMORY;
			}
		}
		else
		{
			output_data = new_out_buffer;
			out_buffer_size = new_out_buffer_size;
		}
#if defined(LIBDEFLATE)
		result = libdeflate_zlib_decompress(decompressor, deflate_stream, in_buffer_size, output_data, out_buffer_size, actual_out_size);
#elif defined(ZLIB)
		result = uncompress(output_data, actual_out_size, deflate_stream, in_buffer_size);
#elif defined(ISAL)
		state_ptr->next_in = deflate_stream;
		state_ptr->avail_in = in_buffer_size;
		state_ptr->next_out = output_data;
		state_ptr->avail_out = out_buffer_size;
		result = isal_inflate_stateless(state_ptr);
#endif
	}
#if defined(LIBDEFLATE)
	libdeflate_free_decompressor(decompressor);

	if (result == LIBDEFLATE_BAD_DATA)
#elif defined(ZLIB)
	if (result == Z_DATA_ERROR)
#elif defined(ISAL)
	if (result != ISAL_DECOMP_OK)
#endif
	{
		fprintf(stderr, "A decompression error has occurred. The data is invalid!\n");
		free(deflate_stream);
		free(output_data);
		return ERROR_INVALID_DATA;
	}
#if defined(ZLIB)
	if (result == Z_MEM_ERROR)
	{
		fprintf(stderr, "Not enough memory resources!\n");
		free(deflate_stream);
		free(output_data);
		return ERROR_NOT_ENOUGH_MEMORY;
	}
#endif

#if defined(ISAL)
	out_buffer_size = state_ptr->total_out;
#else
	out_buffer_size = *actual_out_size;
#endif
	unsigned char *new_output_data = realloc(output_data, out_buffer_size);
	if (!new_output_data)
	{
		fprintf(stderr, "Not enough memory resources!\n");
		free(deflate_stream);
		free(output_data);
		return ERROR_NOT_ENOUGH_MEMORY;
	}
	output_data = new_output_data;

	// The effect of filters reversing
	unsigned char *image_data = malloc(out_buffer_size - image_height);
	if (!image_data)
	{
		fprintf(stderr, "Not enough memory resources!\n");
		free(output_data);
		free(deflate_stream);
		return ERROR_NOT_ENOUGH_MEMORY;
	}
	size_t pixel_size = color_type == 0 ? 1 : 3;
	size_t scanline_size = image_width * pixel_size;
	for (size_t i = 0; i < image_height; i++)
	{
		unsigned char filter_type = output_data[i * (scanline_size + 1)];
		// Filter Type 0: None
		if (filter_type == 0)
		{
			for (size_t j = 0; j < scanline_size; j++)
				image_data[i * scanline_size + j] = output_data[i * (scanline_size + 1) + j + 1];
		}
		// Filter Type 1: Sub
		else if (filter_type == 1)
		{
			for (size_t j = 0; j < scanline_size; j++)
			{
				unsigned char raw = (j >= pixel_size ? image_data[i * scanline_size + j - pixel_size] : 0);
				image_data[i * scanline_size + j] = output_data[i * (scanline_size + 1) + j + 1] + raw;
			}
		}
		// Filter Type 2: Up
		else if (filter_type == 2)
		{
			for (size_t j = 0; j < scanline_size; j++)
			{
				unsigned char prior = (i > 0 ? image_data[(i - 1) * scanline_size + j] : 0);
				image_data[i * scanline_size + j] = output_data[i * (scanline_size + 1) + j + 1] + prior;
			}
		}
		// Filter Type 3: Average
		else if (filter_type == 3)
		{
			for (size_t j = 0; j < scanline_size; j++)
			{
				unsigned int raw = (j >= pixel_size ? image_data[i * scanline_size + j - pixel_size] : 0);
				unsigned int prior = (i > 0 ? image_data[(i - 1) * scanline_size + j] : 0);
				unsigned int floor = (raw + prior) >> 1;
				image_data[i * scanline_size + j] = output_data[i * (scanline_size + 1) + j + 1] + floor;
			}
		}
		// Filter Type 4: Paeth
		else if (filter_type == 4)
		{
			for (size_t j = 0; j < scanline_size; j++)
			{
				unsigned int raw_left = (j >= pixel_size ? image_data[i * scanline_size + j - pixel_size] : 0);
				unsigned int prior_above = (i > 0 ? image_data[(i - 1) * scanline_size + j] : 0);
				unsigned int prior_upper_left = (i == 0 || j < pixel_size ? 0 : image_data[(i - 1) * scanline_size + j - pixel_size]);
				image_data[i * scanline_size + j] =
					output_data[i * (scanline_size + 1) + j + 1] + paeth_predictor(raw_left, prior_above, prior_upper_left);
			}
		}
		// Invalid Filter Type
		else
		{
			fprintf(stderr, "An invalid filter type has encountered. The data is invalid!\n");
			free(deflate_stream);
			free(output_data);
			free(image_data);
			return ERROR_INVALID_DATA;
		}
	}

	FILE *out = fopen(argv[2], "wb");
	if (!out)
	{
		fprintf(stderr, "Cannot find the output file: %s", argv[2]);
		free(deflate_stream);
		free(output_data);
		free(image_data);
		return ERROR_FILE_NOT_FOUND;
	}
	if (color_type == 0)
		fprintf(out, "P5\n");
	else
		fprintf(out, "P6\n");
	fprintf(out, "%u %u\n255\n", image_width, image_height);
	fwrite(image_data, 1, scanline_size * image_height, out);

	fclose(out);
	free(deflate_stream);
	free(output_data);
	free(image_data);
	return 0;
}