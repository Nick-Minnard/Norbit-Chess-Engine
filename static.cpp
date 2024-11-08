
//  CREDIT to Move Encoding System: BBC - Engine by Code Monkey King
//  ================================================================
//  0000 0000 0000 0000 0011 1111    home square
//  0000 0000 0000 1111 1100 0000    target square
//  0000 0000 1111 0000 0000 0000    piece
//  0000 1111 0000 0000 0000 0000    promotion flag
//  0001 0000 0000 0000 0000 0000    capture flag
//  0010 0000 0000 0000 0000 0000    double push flag
//  0100 0000 0000 0000 0000 0000    enpassant flag
//  1000 0000 0000 0000 0000 0000    castling flag

#define encode(homesq, trgtsq, piece, pmt_flg, cpt_flg, dbl_flg, ep_flg, cst_flg)   \
	(homesq) | (trgtsq << 6) | (piece << 12) | (pmt_flg << 16) | (cpt_flg << 20) |  \
	(dbl_flg << 21) | (ep_flg << 22) | (cst_flg << 23)

/**********************************************************************************\
 *
 *                             Attacks
 *
\**********************************************************************************/

void populate_squares_to_edge() {
  for (int square = 0; square < 64; square++) {
    int rank = square / 8;
    int file = square % 8;
    for (int dir = 0; dir < 8; dir++) { squares_to_edge[square][dir] = 0; }
	// N, E, S, W, NE, SE, SW, NW
    squares_to_edge[square][0] = rank;
    squares_to_edge[square][1] = 7 - file;
    squares_to_edge[square][2] = 7 - rank;
    squares_to_edge[square][3] = file;
    squares_to_edge[square][4] = min(rank, 7 - file);
    squares_to_edge[square][5] = min(7 - rank, 7 - file);
    squares_to_edge[square][6] = min(7 - rank, file);
    squares_to_edge[square][7] = min(rank, file);
  }
}

// [king * 64 + enemy] path from king to enemy including the enemy
void calculate_pin_rays() {
	memset(pin_rays, 0ULL, 4096);
	for(int king_square = 0; king_square < 64; king_square++) {
		for(int enemy_square = 0; enemy_square < 64; enemy_square++) {
			for(int dir = 0; dir < 8; dir++) {
				U64 mask = 0ULL;
				bool found = false;
				for(int i = 1; i <= squares_to_edge[king_square][dir]; i++) {
					int square = king_square + direction_offsets[dir] * i;
					mask |= 1ULL << square;
					if(square == enemy_square) { pin_rays[king_square * 64 + enemy_square] = mask; found = true; break; }
				} if(found) { break; }
			}
		}
	}
}

// [king * 64 + enemy] path from enemy to square behind king
void calculate_check_rays() {
	memset(check_rays, 0ULL, 4096);
	for(int king_square = 0; king_square < 64; king_square++) {
		for(int enemy_square = 0; enemy_square < 64; enemy_square++) {
			for(int dir = 0; dir < 8; dir++) {
				U64 mask = 0ULL;
				bool found = false;
				U64 xray_square_mask = 1ULL << (king_square - direction_offsets[dir]);
				if(king_attacks[king_square] & xray_square_mask) { mask |= xray_square_mask; }
				for(int i = 0; i <= squares_to_edge[king_square][dir]; i++) {
					int square = king_square + direction_offsets[dir] * i;
					if(square == enemy_square) { check_rays[king_square * 64 + enemy_square] = mask; found = true; break; }
					mask |= 1ULL << square;
				} if(found) { break; }
			}
		}
	}
}

U64 mask_white_pawn_attacks(int square) {
    U64 bb = 1ULL << square;
    U64 attacks = 0ULL;
    if ((bb >> 7) & not_a_file) { attacks |= (bb >> 7); } // North east
    if ((bb >> 9) & not_h_file) { attacks |= (bb >> 9); } // North west
    return attacks;
}

U64 mask_black_pawn_attacks(int square) {
    U64 bb = 1ULL << square;
    U64 attacks = 0ULL;
    if ((bb << 9) & not_a_file) { attacks |= (bb << 9); } // South east
    if ((bb << 7) & not_h_file) { attacks |= (bb << 7); } // South west
    return attacks;
}

U64 mask_knight_attacks(int square) {
    U64 bb = 1ULL << square;
    U64 attacks = 0ULL;
    if ((bb >> 17) & not_h_file ) { attacks |= (bb >> 17); } // u + l
    if ((bb >> 15) & not_a_file ) { attacks |= (bb >> 15); } // u + r
    if ((bb >> 10) & not_hg_file) { attacks |= (bb >> 10); } // l + u
    if ((bb >> 6)  & not_ab_file) { attacks |= (bb >> 6 ); } // r + u
    if ((bb << 6)  & not_hg_file) { attacks |= (bb << 6 ); } // l + d
    if ((bb << 10) & not_ab_file) { attacks |= (bb << 10); } // r + d
    if ((bb << 15) & not_h_file ) { attacks |= (bb << 15); } // d + l
    if ((bb << 17) & not_a_file ) { attacks |= (bb << 17); } // d + r
    return attacks;
}

U64 mask_king_attacks(int square) {
    U64 bb = 1ULL << square;
    U64 attacks = 0ULL;
    if (bb >> 8) { attacks |= (bb >> 8); } // North
    if (bb << 8) { attacks |= (bb << 8); } // South
    if ((bb >> 1) & not_h_file) { attacks |= (bb >> 1); } // West
    if ((bb << 1) & not_a_file) { attacks |= (bb << 1); } // East
    if ((bb >> 9) & not_h_file) { attacks |= (bb >> 9); } // North west
    if ((bb << 7) & not_h_file) { attacks |= (bb << 7); } // South west
    if ((bb >> 7) & not_a_file) { attacks |= (bb >> 7); } // North east
    if ((bb << 9) & not_a_file) { attacks |= (bb << 9); } // South east
    return attacks;
}

U64 mask_bishop_attacks(int square) {
    U64 attacks = 0ULL;
    int r, f;
    int tr = square / 8;
    int tf = square % 8;
    for (r = tr - 1, f = tf + 1; r >= 1 && f <= 6; r--, f++) { attacks |= (1ULL << (r * 8 + f)); } // North east
    for (r = tr + 1, f = tf + 1; r <= 6 && f <= 6; r++, f++) { attacks |= (1ULL << (r * 8 + f)); } // South east
    for (r = tr - 1, f = tf - 1; r >= 1 && f >= 1; r--, f--) { attacks |= (1ULL << (r * 8 + f)); } // North west
    for (r = tr + 1, f = tf - 1; r <= 6 && f >= 1; r++, f--) { attacks |= (1ULL << (r * 8 + f)); } // South west
    return attacks;
}

U64 mask_rook_attacks(int square) {
    U64 attacks = 0ULL;
    int r, f;
    int tr = square / 8;
    int tf = square % 8;
    for (r = tr - 1; r >= 1; r--) { attacks |= (1ULL << (r * 8 + tf)); } // North
    for (r = tr + 1; r <= 6; r++) { attacks |= (1ULL << (r * 8 + tf)); } // South
    for (f = tf - 1; f >= 1; f--) { attacks |= (1ULL << (tr * 8 + f)); } // West
    for (f = tf + 1; f <= 6; f++) { attacks |= (1ULL << (tr * 8 + f)); } // East
    return attacks;
}

U64 mask_extended_bishop_attacks(int square) {
    U64 attacks = 0ULL;
    int r, f;
    int tr = square / 8;
    int tf = square % 8;
    for (r = tr - 1, f = tf + 1; r >= 0 && f <= 7; r--, f++) { attacks |= (1ULL << (r * 8 + f)); } // North east
    for (r = tr + 1, f = tf + 1; r <= 7 && f <= 7; r++, f++) { attacks |= (1ULL << (r * 8 + f)); } // South east
    for (r = tr - 1, f = tf - 1; r >= 0 && f >= 0; r--, f--) { attacks |= (1ULL << (r * 8 + f)); } // North west
    for (r = tr + 1, f = tf - 1; r <= 7 && f >= 0; r++, f--) { attacks |= (1ULL << (r * 8 + f)); } // South west
    return attacks;
}

U64 mask_extended_rook_attacks(int square) {
    U64 attacks = 0ULL;
    int r, f;
    int tr = square / 8;
    int tf = square % 8;
    for (r = tr - 1; r >= 0; r--) { attacks |= (1ULL << (r * 8 + tf)); } // North
    for (r = tr + 1; r <= 7; r++) { attacks |= (1ULL << (r * 8 + tf)); } // South
    for (f = tf - 1; f >= 0; f--) { attacks |= (1ULL << (tr * 8 + f)); } // West
    for (f = tf + 1; f <= 7; f++) { attacks |= (1ULL << (tr * 8 + f)); } // East
    return attacks;
}

void compute_leaper_attacks() {
    // for (int square = 0; square < 64; square++) {
        // pawn_attacks[white][square] = mask_white_pawn_attacks(square);
        // pawn_attacks[black][square] = mask_black_pawn_attacks(square);
        // knight_attacks[square] = mask_knight_attacks(square);
        // king_attacks[square] = mask_king_attacks(square);
    // }
}

U64 set_occupancy(int index, U64 attack_mask) {
    U64 occupancy = 0ULL;
    int bit_count =  __builtin_popcountll(attack_mask);
    for(int count = 0; count < bit_count; count++) {
        int square = __builtin_ctzll(attack_mask);
        attack_mask &= attack_mask - 1;
        if (index & (1 << count)) { occupancy |= (1ULL << square); }
    } return occupancy;
}

U64 get_bishop_rays(int square, U64 blockers) {
    U64 attacks = 0ULL;
    int tr = square / 8;
    int tf = square % 8;

    auto generate_attacks = [&](int dr, int df) {
        for (int r = tr + dr, f = tf + df; r >= 0 && r <= 7 && f >= 0 && f <= 7; r += dr, f += df) {
            U64 target = 1ULL << (r * 8 + f);
            attacks |= target;
            if (target & blockers) {
                break;
            }
        }
    };

    generate_attacks(-1, 1);  // North east
    generate_attacks(1, 1);   // South east
    generate_attacks(-1, -1); // North west
    generate_attacks(1, -1);  // South west

    return attacks;
}

U64 get_rook_rays(int square, U64 blockers) {
    U64 attacks = 0ULL;
    int tr = square / 8;
    int tf = square % 8;

    auto generate_attacks = [&](int dr, int df) {
        for (int r = tr + dr, f = tf + df; r >= 0 && r <= 7 && f >= 0 && f <= 7; r += dr, f += df) {
            U64 target = 1ULL << (r * 8 + f);
            attacks |= target;
            if (target & blockers) {
                break;
            }
        }
    };

    generate_attacks(-1, 0); // North
    generate_attacks(1, 0);  // South
    generate_attacks(0, -1); // West
    generate_attacks(0, 1);  // East

    return attacks;
}

void compute_slider_attacks(int bishop) {
    for (int square = 0; square < 64; square++) {
        // bishop_masks[square] = mask_bishop_attacks(square);
        // rook_masks[square] = mask_rook_attacks(square);
		// extended_bishop_masks[square] = mask_extended_bishop_attacks(square);
		// extended_rook_masks[square] = mask_extended_rook_attacks(square);
        U64 attack_mask = bishop ? bishop_masks[square] : rook_masks[square];
        int relevant_bit_count = __builtin_popcountll(attack_mask);
        int occupancy_indicies = 1 << relevant_bit_count;
        for (int index = 0; index < occupancy_indicies; index++) {
            U64 occupancy = set_occupancy(index, attack_mask);
            if (bishop) {
                int magic_index = (occupancy * magic_bishop_numbers[square]) >> (64 - relevant_bishop_bits[square]);
                bishop_attacks[square][magic_index] = get_bishop_rays(square, occupancy);
            } else {
                int magic_index = (occupancy * magic_rook_numbers[square]) >> (64 - relevant_rook_bits[square]);
                rook_attacks[square][magic_index] = get_rook_rays(square, occupancy);
            }
        }
    }
}

/**********************************************************************************\
 *
 *                             Magics
 *
\**********************************************************************************/

#define get_random_U64()                                                    \
	((U64)rand() | ((U64)rand() << 16) |                                    \
	((U64)rand() << 32) | ((U64)rand() << 48))

void set_hash_keys() {
    for (int piece = P; piece <= k; piece++) {
        for (int square = 0; square < 64; square++) {
            piece_keys[piece][square] = get_random_U64();
        }
    }
    for (int square = 0; square < 64; square++) {
        ep_keys[square] = get_random_U64();
    }
    for (int index = 0; index < 16; index++) {
        castling_keys[index] = get_random_U64();
    }
    color_key = get_random_U64();
}

U64 random_U64() {
    U64 r1, r2, r3, r4;
    r1 = (U64)(rand()) & 0xFFFF; r2 = (U64)(rand()) & 0xFFFF;
    r3 = (U64)(rand()) & 0xFFFF; r4 = (U64)(rand()) & 0xFFFF;
    return r1 | (r2 << 16) | (r3 << 32) | (r4 << 48);
}

U64 random_U64_fewbits() {
    return random_U64() & random_U64() & random_U64();
}

U64 find_magic(int square, int relevant_bits, int is_bishop) {
    U64 blockers[4096], attacks[4096], used[4096], magic;
    int fail;
    U64 mask = is_bishop ? mask_bishop_attacks(square) : mask_rook_attacks(square);

    // Calculate the number of relevant bits in the attack mask
    int num_bits = __builtin_popcountll(mask);

    // Generate all possible occupancy variations and their corresponding attacks
    for (int i = 0; i < (1 << num_bits); i++) {
        blockers[i] = set_occupancy(i, mask);
        attacks[i] = is_bishop ? get_bishop_rays(square, blockers[i]) : get_rook_rays(square, blockers[i]);
    }

    for (int _ = 0; _ < 100000000; _++) {
        magic = get_random_U64();

        // Skip inappropriate magic numbers
        if (__builtin_popcountll((mask * magic) & 0xFF00000000000000ULL) < 6) { continue; }

        // Initialize used attacks array
        for (int i = 0; i < 4096; i++) { used[i] = 0ULL; }

        // Check each occupancy index to find a valid magic number
        for (int i = 0, fail = 0; !fail && i < (1 << num_bits); i++) {
            int j = (int)((blockers[i] * magic) >> (64 - relevant_bits));

            // Test magic number
            if (used[j] == 0ULL) {
                used[j] = attacks[i];
            } else if (used[j] != attacks[i]) {
                fail = 1;
            }
        }

        if (!fail) {
            return magic;
        }
    }
    return 0ULL;
}

int get_magic_set_size(vector<U64>& magic_array) {
    int size = 0;
    for(int i = 0; i < 64; i++) {
        size += __builtin_popcountll(magic_array[i]);
    } return size;
}

void generate_magics(vector<U64>& magic_array, int n, int is_bishop) {

    // Generate the first round of magics
    for (int square = 0; square < 64; square++) {
        U64 magic = find_magic(square, is_bishop ? relevant_bishop_bits[square] : relevant_rook_bits[square], is_bishop);
        magic_array[square] = magic;
    } cout << "Starting Size: " << get_magic_set_size(magic_array) << " bits" << endl;

    // Refine the magics n times
    for(int i = 0; i < n; i++) {
        for (int square = 0; square < 64; square++) {
            U64 new_magic = find_magic(square, is_bishop ? relevant_bishop_bits[square] : relevant_rook_bits[square], is_bishop);
            int new_magic_size = __builtin_popcountll(new_magic);
            if (__builtin_popcountll(magic_array[square]) > new_magic_size) {
                magic_array[square] = new_magic;
            }
        }
        cout << "#" << i+1 << ": " << get_magic_set_size(magic_array) << " bits" << endl;
    }
}

void compute_magic_numbers(int refinement) {
    vector<U64> rook_magics(64);
    vector<U64> bishop_magics(64);
    generate_magics(rook_magics, refinement, rook);
    generate_magics(bishop_magics, refinement, bishop);
    cout << endl << endl << "Rook Magics:" << endl;
    for (int i = 0; i < 64; i++) { printf("0x%llxULL,", rook_magics[i]); }
    cout << endl << endl << "Bishop Magics:" << endl;
    for (int i = 0; i < 64; i++) { printf("0x%llxULL,", bishop_magics[i]); }
    cout << endl << endl;
}

void regenerate_inverse_position_scores() {
	const int invert_offsets[8] = {56, 40, 24, 8, -8, -24, -40, -56};
	for(int square = 0; square < 64; square++) {
		position_scores[6 ][square] = -position_scores[0][square + invert_offsets[square / 8]];
		position_scores[7 ][square] = -position_scores[1][square + invert_offsets[square / 8]];
		position_scores[8 ][square] = -position_scores[2][square + invert_offsets[square / 8]];
		position_scores[9 ][square] = -position_scores[3][square + invert_offsets[square / 8]];
		position_scores[11][square] = -position_scores[5][square + invert_offsets[square / 8]];
	} for(int i = 0; i < 6; i++) {
		cout << "{";
		for(int j = 0; j < 64; j++) {
			cout << position_scores[6 + i][j] << ",";
		} cout << "}," << endl;
	}
}