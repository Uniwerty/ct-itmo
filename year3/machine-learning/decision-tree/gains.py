import numpy as np


def get_gain(criterion):
    match criterion:
        case 'entropy':
            return information_gain
        case 'gini':
            return gini_gain
        case _:
            raise Exception('Unknown gain specified: entropy or gini are available')


def information_gain(main_positives, main_negatives, left_positives, left_negatives):
    return calculate_gain(entropy, main_positives, main_negatives, left_positives, left_negatives)


def gini_gain(main_positives, main_negatives, left_positives, left_negatives):
    return calculate_gain(gini_index, main_positives, main_negatives, left_positives, left_negatives)


def calculate_gain(criterion, main_positives, main_negatives, left_positives, left_negatives):
    main_samples = main_positives + main_negatives
    left_samples = left_positives + left_negatives
    right_samples = main_samples - left_samples
    return (criterion(main_positives / main_samples, main_negatives / main_samples)
            - left_samples / main_samples * criterion(left_positives / left_samples, left_negatives / left_samples)
            - right_samples / main_samples * criterion((main_positives - left_positives) / right_samples,
                                                       (main_negatives - left_negatives) / right_samples))


def entropy(positive_p, negative_p):
    return -positive_p * (np.log2(positive_p) if positive_p > 0 else 0) - negative_p * (np.log2(negative_p) if negative_p > 0 else 0)


def gini_index(positive_p, negative_p):
    return 1 - positive_p * positive_p - negative_p * negative_p
