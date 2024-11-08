/***************************************************************************************************************************************\
 *
 * 						  Norbit: A chess engine by Nick Minnard
 * 
 *                        Started October 1st, 2023
 *  
\***************************************************************************************************************************************/

#include <windows.h>
#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include <chrono>
#include "globals.hpp"

using namespace std;

/**********************************************************************************\
 *
 *                             Print Functions
 *
\**********************************************************************************/

void print_bitboard(U64 bitboard) {
    for (int rank = 0; rank < 8; rank++) {
        for (int file = 0; file < 8; file++) {
            int square = rank * 8 + file;
            if (!file) { cout << 8 - rank << "   "; }
            cout << ((bitboard & (1ULL << square)) ? 1 : 0) << "  ";
        } cout << endl;
    } cout << endl << "    a  b  c  d  e  f  g  h" << endl << endl;
    printf("    Bitboard: %llud\n", bitboard);
}

void print_board() {
    char ascii_pieces[] = "PNBRQKpnbrqk";
    for (int rank = 0; rank < 8; rank++) {
        for (int file = 0; file < 8; file++) {
            int square = rank * 8 + file;
            if (!file) { cout << 8 - rank << "   "; }
            int piece = -1;
            for (int p_type = P; p_type <= k; p_type++) {
                if (bitboards[p_type] & (1ULL << square)) { piece = p_type; }
            } cout << ((piece == -1) ? '.' : ascii_pieces[piece]) << "  ";
        } cout << endl;
    } cout << endl << "    a  b  c  d  e  f  g  h" << endl << endl;
    cout << "    Side: " << (!color ? "white" : "black") << endl;
    cout << "    En Passant: " << ((ep_square == unset) ? "-" : idx_to_name[ep_square]) << endl;
    cout << "    Castling: " << ((castling_rights & wk) ? "K" : "-") << ((castling_rights & wq) ? "Q" : "-")
         << ((castling_rights & bk) ? "k" : "-") << ((castling_rights & bq) ? "q" : "-") << endl;
    printf("    Hash Key: %llx\n", pos_hash);
}

void print_move(int move) {
    string promoted_pieces[] = {"", "n", "b", "r", "q", "", "", "n", "b", "r", "q"};
    cout << idx_to_name[decode_homesq(move)];
    cout << idx_to_name[decode_trgtsq(move)];
    cout << promoted_pieces[decode_pmt_flg(move)];
}

void print_move_list(MoveGroup *move_group) {
    for (int move_count = 0; move_count < move_group->count; move_count++) {
        int move = move_group->moves[move_count];
        print_move(move);
        cout << endl;
    } cout << endl;
}

/**********************************************************************************\
 *
 *                             Fen Parsing
 *
\**********************************************************************************/

int char_to_piece(char c) {
    switch (c) {
        case 'P': return P; case 'N': return N;
        case 'B': return B; case 'R': return R;
        case 'Q': return Q; case 'K': return K;
        case 'p': return p; case 'n': return n;
        case 'b': return b; case 'r': return r;
        case 'q': return q; case 'k': return k;
        default : return -1;
    }
}

U64 generate_hash() {
    U64 h = 0ULL; U64 bb;
    for (int piece = P; piece <= k; piece++) {
        bb = bitboards[piece];
        while (bb) {
            int square = __builtin_ctzll(bb);
            h ^= piece_keys[piece][square];
            bb &= bb - 1;
        }
    } if (ep_square != unset) { h ^= ep_keys[ep_square]; }
    h ^= castling_keys[castling_rights];
    if (color) { h ^= color_key; }
    return h;
}

void reset_board_state() {
    memset(bitboards, 0ULL, sizeof(bitboards));
    memset(occupancies, 0ULL, sizeof(occupancies));
    ep_square = unset;
    castling_rights = 0;
	memset(repetition_table, 0, sizeof(repetition_table));
	repetition_index = 1;
	fifty = 0;
}

vector<string> parse_fen_sections(const string& fen) {
    vector<string> sections;
    string section;
    for (int i = 0; i < fen.size(); i++) {
        if (fen[i] == ' ') {
            sections.push_back(section);
            section = "";
        } else { section += fen[i]; }
    } sections.push_back(section);
    return sections;
}

void parse_pieces(const string& piece_section) {
    int file = 0; int rank = 0;
    for (int i = 0; i < piece_section.size(); i++) {
        int square = rank * 8 + file;
        if (piece_section[i] == '/') {
            file = 0; rank++;
        } else if (isdigit(piece_section[i])) {
            file += piece_section[i] - '0';
        } else {
            bitboards[char_to_piece(piece_section[i])] |= (1ULL << square);
            file++;
        }
    }
}

void parse_castling_rights(const string& castling_section) {
    for (int i = 0; i < castling_section.size(); i++) {
        switch (castling_section[i]) {
            case 'K': castling_rights |= wk; break;
            case 'Q': castling_rights |= wq; break;
            case 'k': castling_rights |= bk; break;
            case 'q': castling_rights |= bq; break;
            case '-': break;
        }
    }
}

void parse_ep_square(const string& ep_section) {
    if (ep_section != "-") {
        for(int square = 0; square < 64; square++) {
            if(ep_section == idx_to_name[square]) { ep_square = square; }
        }
    }
}

void populate_occupancies() {
    for (int piece = P; piece <= K; piece++) { occupancies[white] |= bitboards[piece]; }
    for (int piece = p; piece <= k; piece++) { occupancies[black] |= bitboards[piece]; }
    occupancies[both] |= occupancies[white];
    occupancies[both] |= occupancies[black];
}

void parse_fen(const string& fen) {
    reset_board_state();
    vector<string> sections = parse_fen_sections(fen);
    parse_pieces(sections[0]);
    color = (sections[1] == "w") ? white : black;
    parse_castling_rights(sections[2]);
    parse_ep_square(sections[3]);
	if(sections.size() > 4) { fifty = stoi(sections[4]); }
    populate_occupancies();
    pos_hash = generate_hash();
	repetition_table[0] = pos_hash;
}

/**********************************************************************************\
 *
 *                             Move Handling
 *
\**********************************************************************************/

#define copy_board_state()                                                  \
	U64 bitboards_copy[12], occupancies_copy[3];                            \
	int color_copy, ep_square_copy, castling_rights_copy, fifty_copy;       \
	__builtin_memcpy(bitboards_copy, bitboards, 96);                        \
	__builtin_memcpy(occupancies_copy, occupancies, 24);                    \
	color_copy = color, ep_square_copy = ep_square, fifty_copy = fifty;     \
	castling_rights_copy = castling_rights; U64 pos_hash_copy = pos_hash;

#define revert_board_state()                                                \
	__builtin_memcpy(bitboards, bitboards_copy, 96);                        \
	__builtin_memcpy(occupancies, occupancies_copy, 24);                    \
	color = color_copy, ep_square = ep_square_copy, fifty = fifty_copy;     \
	castling_rights = castling_rights_copy; pos_hash = pos_hash_copy;

inline void move_piece(int homesq, int trgtsq, int piece) {
    bitboards[piece] &= ~(1ULL << homesq);
    bitboards[piece] |= (1ULL << trgtsq);
    pos_hash ^= piece_keys[piece][homesq];
    pos_hash ^= piece_keys[piece][trgtsq];
	if(piece == P || piece == p) { fifty = 0; }
}

inline void handle_capture(int trgtsq) {
    for (int piece = (color ? P : p); piece <= (color ? Q : q); piece++) {
        if (bitboards[piece] & (1ULL << trgtsq)) {
            bitboards[piece] &= ~(1ULL << trgtsq);
            pos_hash ^= piece_keys[piece][trgtsq];
            break;
        }
    }
}

inline void handle_promotion(int trgtsq, int pmt_piece) {
    bitboards[color ? p : P] &= ~(1ULL << trgtsq);
    pos_hash ^= piece_keys[!color ? P : p][trgtsq];
    bitboards[pmt_piece] |= (1ULL << trgtsq);
    pos_hash ^= piece_keys[pmt_piece][trgtsq];
}

inline void handle_enpassant(int ep_flag, int dbl_flag, int trgtsq) {
    if (ep_flag) {
        const int opposing_pawn = color ? P : p;
        const int capture_square = color ? (trgtsq - 8) : (trgtsq + 8);
        bitboards[opposing_pawn] &= ~(1ULL << capture_square);
        pos_hash ^= piece_keys[opposing_pawn][capture_square];
    } if (ep_square != unset) {
        pos_hash ^= ep_keys[ep_square];
        ep_square = unset;
    } if (dbl_flag) {
        ep_square = color ? (trgtsq - 8) : (trgtsq + 8);
        pos_hash ^= ep_keys[ep_square];
    }
}

inline void handle_castling(const int king_to) {
    int homesq, trgtsq;
    const int rook = color ? r : R;
    switch (king_to) {
        case g1: homesq = h1; trgtsq = f1; break;
        case c1: homesq = a1; trgtsq = d1; break;
        case g8: homesq = h8; trgtsq = f8; break;
        case c8: homesq = a8; trgtsq = d8; break;
    } bitboards[rook] &= ~(1ULL << homesq);
    bitboards[rook] |= (1ULL << trgtsq);
    pos_hash ^= piece_keys[rook][homesq];
    pos_hash ^= piece_keys[rook][trgtsq];
}

inline void update_castling_rights(int homesq, int trgtsq) {
    pos_hash ^= castling_keys[castling_rights];
    castling_rights &= castling_mask[homesq];
    castling_rights &= castling_mask[trgtsq];
    pos_hash ^= castling_keys[castling_rights];
}

inline void update_occupancies() {
    occupancies[white] = 0ULL; occupancies[black] = 0ULL;
    for (int piece = P; piece <= K; piece++) { occupancies[white] |= bitboards[piece]; }
    for (int piece = p; piece <= k; piece++) { occupancies[black] |= bitboards[piece]; }
    occupancies[both] = occupancies[white] | occupancies[black];
}

inline int make_move(int move) {
    const int homesq = decode_homesq(move);
    const int trgtsq = decode_trgtsq(move);
	fifty++;
    move_piece(homesq, trgtsq, decode_piece(move));
    if (decode_cpt_flg(move)) { handle_capture(trgtsq); fifty = 0; }
    if (int promoted_piece = decode_pmt_flg(move)) { handle_promotion(trgtsq, promoted_piece); }
    handle_enpassant(decode_ep_flg(move), decode_dbl_flg(move), trgtsq);
    if (decode_cst_flg(move)) { handle_castling(trgtsq); }
    update_occupancies();
    color ^= 1; pos_hash ^= color_key;
    update_castling_rights(homesq, trgtsq);
    return 1;
}

/**********************************************************************************\
 *
 *                             Move Generation
 *
\**********************************************************************************/

inline void append_promotion_moves(MoveGroup *move_group, int homesq, int trgtsq, int cpt) {
	move_group->add(encode(homesq, trgtsq, fpawn, (fqueen), cpt, 0, 0, 0));
	move_group->add(encode(homesq, trgtsq, fpawn, (frook), cpt, 0, 0, 0));
	move_group->add(encode(homesq, trgtsq, fpawn, (fbishop), cpt, 0, 0, 0));
	move_group->add(encode(homesq, trgtsq, fpawn, (fknight), cpt, 0, 0, 0));
}

inline U64 get_bishop_attacks(int square, U64 blockers) {
  	return bishop_attacks[square][(blockers & bishop_masks[square]) * magic_bishop_numbers[square] >> (64 - relevant_bishop_bits[square])];
}

inline U64 get_rook_attacks(int square, U64 blockers) {
  	return rook_attacks[square][(blockers & rook_masks[square]) * magic_rook_numbers[square] >> (64 - relevant_rook_bits[square])];
}

inline U64 shift_backward_one(U64 mask) { return color ? (mask >> 8) : (mask << 8); }

inline U64 shift_backward_two(U64 mask) { return color ? (mask >> 16) : (mask << 16); }

inline U64 shift_back_right(U64 mask) { return color ? (mask >> 9) : (mask << 9); }

inline U64 shift_back_left(U64 mask) { return color ? (mask >> 7) : (mask << 7); }

inline void compute_pawn_data() {
	U64 attacks = shift_back_right(bitboards[epawn] & not_right[color]) | shift_back_left(bitboards[epawn] & not_left[color]);
	if(bitboards[fking] & attacks) {
		if(check_mask) { in_double_check = true; }
		check_mask |= pawn_attacks[color][friendly_king_square] & bitboards[epawn];
	} attack_mask |= attacks;
}

inline void compute_knight_data() {
	U64 opponent_knights = bitboards[eknight];
	U64 checkers = knight_attacks[friendly_king_square] & opponent_knights;
	if(checkers) {
		if(check_mask) { in_double_check = true; }
		check_mask |= checkers;
	} bit_loop(opponent_knights) { attack_mask |= knight_attacks[__builtin_ctzll(opponent_knights)]; }
}

inline bool search_slider_attack(int square) {
	if((get_rook_attacks(square, occupancies[both]) & (bitboards[erook] | bitboards[equeen])) ||
	(get_bishop_attacks(square, occupancies[both]) & (bitboards[ebishop] | bitboards[equeen]))) {
		attack_mask |= 1ULL << square; return true;
	} return false;
}

inline void compute_slider_data() {
	U64 xray_osliders = extended_rook_masks[friendly_king_square] & (bitboards[erook] | bitboards[equeen]);
	U64 xray_dsliders = extended_bishop_masks[friendly_king_square] & (bitboards[ebishop] | bitboards[equeen]);
	bit_loop(xray_osliders) {
		const int oslider = __builtin_ctzll(xray_osliders);
		U64 pin_mask = pin_rays[friendly_king_square * 64 + oslider];
		const int occ_cnt = __builtin_popcountll(pin_mask & occupancies[both]);
		if(occ_cnt == 1) {
			if(check_mask) { in_double_check = true; }
			check_mask |= pin_mask; attack_mask |= check_rays[friendly_king_square * 64 + oslider];
		} else if(occ_cnt == 2 && pin_mask & occupancies[color]) { opin |= pin_mask; }
	} bit_loop(xray_dsliders) {
		const int dslider = __builtin_ctzll(xray_dsliders);
		U64 pin_mask = pin_rays[friendly_king_square * 64 + dslider];
		const int occ_cnt = __builtin_popcountll(pin_mask & occupancies[both]);
		if(occ_cnt == 1) {
			if(check_mask) { in_double_check = true; }
			check_mask |= pin_mask; attack_mask |= check_rays[friendly_king_square * 64 + dslider];
		} else if(occ_cnt == 2 && pin_mask & occupancies[color]) { dpin |= pin_mask; }
	}
}

inline void compute_checks_and_pins() {
	compute_pawn_data();
	compute_knight_data();
	compute_slider_data();
	if(!check_mask) { check_mask = 0xffffffffffffffffULL; }
	movable_square = ~occupancies[color] & check_mask;
}

inline void generate_castling_moves(MoveGroup *move_group) {
	if(color) {
		if(castling_rights & bk) {
			search_slider_attack(g8);
			if(!((occupancies[both] | attack_mask) & 0x60ULL)) { move_group->add(8434052); }
		} if(castling_rights & bq) {
			search_slider_attack(c8);
			if(!(occupancies[both] & 0xeULL) && !(attack_mask & 0xcULL)) { move_group->add(8433796); }
		}
	} else {
		if(castling_rights & wk) {
			search_slider_attack(g1);
			if(!((occupancies[both] | attack_mask) & 0x6000000000000000ULL)) { move_group->add(8413116); }
		} if(castling_rights & wq) {
			search_slider_attack(c1);
			if(!(occupancies[both] & 0xe00000000000000ULL) && !(attack_mask & 0xc00000000000000ULL)) { move_group->add(8412860); }
		}
	}
}

inline void generate_king_moves(MoveGroup *move_group) {
	U64 king_moves = king_attacks[friendly_king_square] & ~occupancies[color] & ~attack_mask;
	bit_loop(king_moves) {
		const int trgtsq = __builtin_ctzll(king_moves);
		if(!search_slider_attack(trgtsq)) {
			const bool is_capture = (1ULL << trgtsq) & occupancies[!color];
			if(gen_quiets || is_capture) { move_group->add(encode(friendly_king_square, trgtsq, fking, 0, is_capture, 0, 0, 0)); }
		}
	} if(!(bitboards[fking] & attack_mask) && gen_quiets) { generate_castling_moves(move_group); }
}

inline bool king_attacked_after_enpassant (int homesq, int cptrsq) {
	U64 post_ep_mask = occupancies[both] & ~((1ULL << homesq) | (1ULL << cptrsq));
	if(get_rook_attacks(friendly_king_square, post_ep_mask) & ep_rank[color] & (bitboards[erook] | bitboards[equeen])) { return true; }
	return false;
}

inline void generate_pawn_moves(MoveGroup *move_group) {
    const U64 dpawns = bitboards[fpawn] & ~opin;
    const U64 opawns = bitboards[fpawn] & ~dpin;
    U64 pawn_masks[4] = { // l - r - f - d
		dpawns & shift_back_right(occupancies[!color] & not_right[color] & check_mask),
		dpawns & shift_back_left(occupancies[!color] & not_left[color] & check_mask),
		opawns & shift_backward_one(~occupancies[both]),
		pawn_masks[2] & pawn_rank[color] & shift_backward_two(~occupancies[both] & check_mask)
	}; pawn_masks[2] &= shift_backward_one(check_mask);
	pawn_masks[0] = (pawn_masks[0] & shift_back_right(dpin & not_right[color])) | (pawn_masks[0] & ~dpin);
	pawn_masks[1] = (pawn_masks[1] & shift_back_left(dpin & not_left[color])) | (pawn_masks[1] & ~dpin);
	pawn_masks[2] = (pawn_masks[2] & shift_backward_one(opin)) | (pawn_masks[2] & ~opin);
	pawn_masks[3] = (pawn_masks[3] & shift_backward_two(opin)) | (pawn_masks[3] & ~opin);
    
	if (ep_square != unset) {
		U64 ep_target_pawn = 1ULL << (ep_square + ep_offsets[color]);
		U64 ep_pawn_masks[2] = {
			dpawns & not_left[color]  & (color ? ((ep_target_pawn & check_mask) >> 1) : ((ep_target_pawn & check_mask) << 1)),
			dpawns & not_right[color] & (color ? ((ep_target_pawn & check_mask) << 1) : ((ep_target_pawn & check_mask) >> 1))
		}; if (ep_pawn_masks[0] | ep_pawn_masks[1]) {
			ep_pawn_masks[0] = (ep_pawn_masks[0] & shift_back_right(dpin & not_right[color])) | (ep_pawn_masks[0] & ~dpin);
			ep_pawn_masks[1] = (ep_pawn_masks[1] & shift_back_left(dpin & not_left[color])) | (ep_pawn_masks[1] & ~dpin);
			for(int i = 0; i < 2; i++) {
				if (ep_pawn_masks[i]) {
					const int homesq = __builtin_ctzll(ep_pawn_masks[i]);
					if(!king_attacked_after_enpassant(homesq, __builtin_ctzll(ep_target_pawn))) {
						move_group->add(encode(homesq, ep_square, fpawn, 0, 1, 0, 1, 0));
					}
				}
			}
		}
	}
	
	for(int i = 0; i < (gen_quiets ? 4 : 2); i++) {
		bit_loop(pawn_masks[i]) {
			const int homesq = __builtin_ctzll(pawn_masks[i]);
			const int trgtsq = homesq + pawn_offsets[color][i];
			if((1ULL << homesq) & pawn_rank[!color]) {
				append_promotion_moves(move_group, homesq, trgtsq, cptr_flg_by_offset[i]);
			} else {
				move_group->add(encode(homesq, trgtsq, fpawn, 0, cptr_flg_by_offset[i], push_flg_by_offset[i], 0, 0));
			}
		}
	}
}

inline void generate_knight_moves(MoveGroup *move_group) {
	U64 knights = bitboards[fknight] & ~(opin | dpin);
	bit_loop(knights) {
		const int homesq = __builtin_ctzll(knights);
		U64 attacks = knight_attacks[homesq] & movable_square;
		bit_loop(attacks) {
			const int trgtsq = __builtin_ctzll(attacks);
			const bool is_capture = (1ULL << trgtsq) & occupancies[!color];
			if(gen_quiets || is_capture) { move_group->add(encode(homesq, trgtsq, fknight, 0, is_capture, 0, 0, 0)); }
		}
	}
}

inline void generate_bishop_moves(MoveGroup *move_group) {
	U64 bishops = bitboards[fbishop] & ~opin;
	U64 pinned = (bishops | bitboards[fqueen]) & dpin;
	U64 unpinned = bishops & ~dpin;
	bit_loop(pinned) {
		const int homesq = __builtin_ctzll(pinned);
		U64 targets = get_bishop_attacks(homesq, occupancies[both]) & movable_square & dpin;
		bit_loop (targets) {
			const int trgtsq = __builtin_ctzll(targets);
			const bool is_capture = occupancies[!color] & (1ULL << trgtsq);
			if(gen_quiets || is_capture) { move_group->add(encode(homesq, trgtsq, ((1ULL << homesq) & bitboards[fqueen] ? fqueen : fbishop), 0, is_capture, 0, 0, 0)); }
		}
	} bit_loop(unpinned) {
		const int homesq = __builtin_ctzll(unpinned);
		U64 targets = get_bishop_attacks(homesq, occupancies[both]) & movable_square;
		bit_loop (targets) {
			const int trgtsq = __builtin_ctzll(targets);
			const bool is_capture = occupancies[!color] & (1ULL << trgtsq);
			if(gen_quiets || is_capture) { move_group->add(encode(homesq, trgtsq, fbishop, 0, is_capture, 0, 0, 0)); }
		}
	}
}

inline void generate_rook_moves(MoveGroup *move_group) {
	U64 rooks = bitboards[frook] & ~dpin;
	U64 pinned = (rooks | bitboards[fqueen]) & opin;
	U64 unpinned = rooks & ~opin;
	bit_loop(pinned) {
		const int homesq = __builtin_ctzll(pinned);
		U64 targets = get_rook_attacks(homesq, occupancies[both]) & movable_square & opin;
		bit_loop (targets) {
			const int trgtsq = __builtin_ctzll(targets);
			const bool is_capture = occupancies[!color] & (1ULL << trgtsq);
			if(gen_quiets || is_capture) { move_group->add(encode(homesq, trgtsq, ((1ULL << homesq) & bitboards[fqueen] ? fqueen : frook), 0, is_capture, 0, 0, 0)); }
		}
	} bit_loop(unpinned) {
		const int homesq = __builtin_ctzll(unpinned);
		U64 targets = get_rook_attacks(homesq, occupancies[both]) & movable_square;
		bit_loop (targets) {
			const int trgtsq = __builtin_ctzll(targets);
			const bool is_capture = occupancies[!color] & (1ULL << trgtsq);
			if(gen_quiets || is_capture) { move_group->add(encode(homesq, trgtsq, frook, 0, is_capture, 0, 0, 0)); }
		}
	}
}

inline void generate_queen_moves(MoveGroup *move_group) {
	U64 queens = bitboards[fqueen] & ~(opin | dpin);
	bit_loop(queens) {
		const int homesq = __builtin_ctzll(queens);
		U64 targets = (get_bishop_attacks(homesq, occupancies[both]) | get_rook_attacks(homesq, occupancies[both])) & movable_square;
		bit_loop (targets) {
			const int trgtsq = __builtin_ctzll(targets);
			const bool is_capture = occupancies[!color] & (1ULL << trgtsq);
			if(gen_quiets || is_capture) { move_group->add(encode(homesq, trgtsq, fqueen, 0, is_capture, 0, 0, 0)); }
		}
	}
}

inline void generate_moves(MoveGroup *move_group, bool include_quiets) {
	if (color) { fpawn=6,fknight=7,fbishop=8,frook=9,fqueen=10,fking=11,epawn=0,eknight=1,ebishop=2,erook=3,equeen=4,eking=5; }
	else { fpawn=0,fknight=1,fbishop=2,frook=3,fqueen=4,fking=5,epawn=6,eknight=7,ebishop=8,erook=9,equeen=10,eking=11; }

	gen_quiets = include_quiets;
	in_double_check = false;
	attack_mask = king_attacks[__builtin_ctzll(bitboards[eking])];
	check_mask = 0ULL; opin = 0ULL; dpin = 0ULL;
	friendly_king_square = __builtin_ctzll(bitboards[fking]);

	compute_checks_and_pins();
	generate_king_moves(move_group);

	if (in_double_check) { return; }

	generate_pawn_moves(move_group);
	generate_knight_moves(move_group);
	generate_bishop_moves(move_group);
	generate_rook_moves(move_group);
	generate_queen_moves(move_group);
}


/**********************************************************************************\
 *
 *                             Evaluation
 *
\**********************************************************************************/

static inline int evaluate() {
	int score = 0;
	U64 bitboard;
	int piece, square;
	for(int bb_piece = P; bb_piece <= k; bb_piece++) {
		bitboard = bitboards[bb_piece];
		bit_loop(bitboard) {
			piece = bb_piece;
			square = __builtin_ctzll(bitboard);
			score += material_score[piece];
			switch (piece) {
				case P: score += pawn_score[square]; break;
				case N: score += knight_score[square]; break;
				case B: score += bishop_score[square]; break;
				case R: score += rook_score[square]; break;
				case K: score += king_score[square]; break;
				case p: score -= pawn_score[mirror_score[square]]; break;
				case n: score -= knight_score[mirror_score[square]]; break;
				case b: score -= bishop_score[mirror_score[square]]; break;
				case r: score -= rook_score[mirror_score[square]]; break;
				case k: score -= king_score[mirror_score[square]]; break;
			}
		}
	} return color ? -score : score;
}

inline bool is_three_fold() {
    int count = 0;
    for(int i = 0; i < repetition_index; i++) {
        if(repetition_table[i] == pos_hash) { count++; if(count >= 3) { return true; } }
    } return false;
}

/**********************************************************************************\
 *
 *                             Move Ordering
 *
\**********************************************************************************/

int ply;

const int mvv_lva[12][12] = {
    {105, 205, 305, 405, 505, 605, 105, 205, 305, 405, 505, 605},
    {104, 204, 304, 404, 504, 604, 104, 204, 304, 404, 504, 604},
    {103, 203, 303, 403, 503, 603, 103, 203, 303, 403, 503, 603},
    {102, 202, 302, 402, 502, 602, 102, 202, 302, 402, 502, 602},
    {101, 201, 301, 401, 501, 601, 101, 201, 301, 401, 501, 601},
    {100, 200, 300, 400, 500, 600, 100, 200, 300, 400, 500, 600},
    {105, 205, 305, 405, 505, 605, 105, 205, 305, 405, 505, 605},
    {104, 204, 304, 404, 504, 604, 104, 204, 304, 404, 504, 604},
    {103, 203, 303, 403, 503, 603, 103, 203, 303, 403, 503, 603},
    {102, 202, 302, 402, 502, 602, 102, 202, 302, 402, 502, 602},
    {101, 201, 301, 401, 501, 601, 101, 201, 301, 401, 501, 601},
    {100, 200, 300, 400, 500, 600, 100, 200, 300, 400, 500, 600}
};

int killer_moves[2][64];
int history_moves[12][64];
int pv_length[64]; int pv_table[64][64];

bool in_pv;

inline int score_move(int move) {
	if(in_pv) { if(pv_table[0][ply] == move) { return 20000; } }
	if(decode_cpt_flg(move)) {
		U64 trgtsq_mask = 1ULL << decode_trgtsq(move);
		for(int enemy = epawn; enemy <= eking; enemy++) {
			if(bitboards[enemy] & trgtsq_mask) { return mvv_lva[decode_piece(move)][enemy] + 10000; }
		}
	} else {
		if(killer_moves[0][ply] == move) { return 9000; }
		else if(killer_moves[1][ply] == move) { return 8000; }
		else { return history_moves[decode_piece(move)][decode_trgtsq(move)]; }
	}
	return 0;
}

inline void order_moves(MoveGroup *move_group, int best_move) {
	vector<int> move_scores;
	for(int i = 0; i < move_group->count; i++) {
		if(best_move == move_group->moves[i]) { move_scores.push_back(30000); }
		else { move_scores.push_back(score_move(move_group->moves[i])); }
	} for(int i = 0; i < move_group->count; i++) {
		for(int j = i + 1; j < move_group->count; j++) {
			if(move_scores[i] < move_scores[j]) {
				int temp_score = move_scores[i]; move_scores[i] = move_scores[j]; move_scores[j] = temp_score;
				int temp_move = move_group->moves[i]; move_group->moves[i] = move_group->moves[j]; move_group->moves[j] = temp_move;
			}
		}
	}
}

/**********************************************************************************\
 *
 *                             Transposition Table
 *
\**********************************************************************************/

int hash_entries = 0;
#define no_hash_entry 100000
#define hash_flag_exact 0
#define hash_flag_alpha 1
#define hash_flag_beta 2
#define mate_value 49000
#define mate_score 48000
typedef struct { U64 hash_key; int depth; int flag; int score; int best_move; } tt;

tt *hash_table = NULL;

void clear_hash_table() {
    tt *hash_entry;
    for(hash_entry = hash_table; hash_entry < hash_table + hash_entries; hash_entry++) {
        hash_entry->hash_key = 0; hash_entry->depth = 0;
        hash_entry->flag = 0; hash_entry->score = 0;
    }
}

void init_hash_table(int mb) {
    int hash_size = 0x100000 * mb;
    hash_entries =  hash_size / sizeof(tt);
    if(hash_table != NULL) { free(hash_table); }
    hash_table = (tt *) malloc(hash_entries * sizeof(tt));

    if(hash_table == NULL) {
        printf("    Couldn't allocate memory for hash table, tryinr %dMB...", mb / 2);
        init_hash_table(mb / 2);
    } else {
        clear_hash_table();
    }
}

static inline int read_hash_entry(int alpha, int beta, int* best_move, int depth) {
    tt *hash_entry = &hash_table[pos_hash % hash_entries];
    if(hash_entry->hash_key == pos_hash) {
        if(hash_entry->depth >= depth) {
            int score = hash_entry->score;
            if(score < -mate_score) { score += ply; }
            if(score > mate_score) { score -= ply; }
    
            if(hash_entry->flag == hash_flag_exact) { return score; }
            if((hash_entry->flag == hash_flag_alpha) && (score <= alpha)) { return alpha; }
            if((hash_entry->flag == hash_flag_beta) && (score >= beta)) { return beta; }
        } *best_move = hash_entry->best_move;
    } return no_hash_entry;
}

static inline void write_hash_entry(int score, int best_move, int depth, int hash_flag) {
    tt *hash_entry = &hash_table[pos_hash % hash_entries];
    if(score < -mate_score) { score -= ply; }
    if(score > mate_score) { score += ply; }
    hash_entry->hash_key = pos_hash;
    hash_entry->score = score;
    hash_entry->flag = hash_flag;
    hash_entry->depth = depth;
    hash_entry->best_move = best_move;
}

/**********************************************************************************\
 *
 *                             Searching
 *
\**********************************************************************************/

inline int get_time() {
    using chrono::duration_cast;
    using chrono::milliseconds;
    using chrono::system_clock;
    return duration_cast<milliseconds>(system_clock::now().time_since_epoch()).count();
}

inline void handle_interrupts() {
	if(get_time() > stop_time) { stop_flag = 1; }
}

inline int quiescence(int alpha, int beta) {
	if(nodes & 16383) { handle_interrupts(); }
	if(stop_flag) { return 0; }
	if(ply > 63) { return evaluate(); }
	nodes++;

	int eval = evaluate();
	if(eval >= beta) { return beta; }
	if(eval > alpha) { alpha = eval; }

	MoveGroup move_group;
	generate_moves(&move_group, 0);
	order_moves(&move_group, 0);
	for(int cnt = 0; cnt < move_group.count; cnt++) {
		copy_board_state(); ply++;
		make_move(move_group.moves[cnt]);
		repetition_table[repetition_index] = pos_hash; repetition_index++;
		int score = -quiescence(-beta, -alpha);
		revert_board_state(); ply--; repetition_index--;
		if(score >= beta) { return beta; }
		if(score > alpha) { alpha = score; }
	} return alpha;
}

inline int negamax(int alpha, int beta, int depth) {
	if(nodes & 16383) { handle_interrupts(); }
	if(stop_flag) { return 0; }
	if(ply > 63) { return evaluate(); }
    nodes++; int score; int best_move = 0; int hash_flag = hash_flag_alpha;
	int pv_node = beta - alpha > 1;

	pv_length[ply] = ply;
	if(is_three_fold() || fifty > 99) { return 0; }
	if(ply && (score = read_hash_entry(alpha, beta, &best_move, depth)) != no_hash_entry && !pv_node) { return score; }
	if(depth == 0) { return quiescence(alpha, beta); }

	MoveGroup move_group;
	generate_moves(&move_group, 1);
	bool in_check = bitboards[fking] & attack_mask;

	if(move_group.count == 0) {
		if(in_check) { return -49000 + ply; }
		else { return 0; }
	} if(in_check) { depth++; }

	if(depth >= 3 && !in_check && ply) {
		copy_board_state(); ply++;
		repetition_table[repetition_index] = pos_hash; repetition_index++;
		color ^= 1; pos_hash ^= color_key;
		if(ep_square != unset) pos_hash ^= ep_keys[ep_square]; ep_square = unset;
		score = -negamax(-beta, -beta + 1, depth - 3);
		revert_board_state(); ply--; repetition_index--;
		if(score >= beta) { return beta; }
	}

    if(!pv_node && !in_check && depth <= 3) {
        score = evaluate() + 125;
        int new_score;
        if(score < beta) {
            if(depth == 1) {
                new_score = quiescence(alpha, beta);
                return new_score > score ? new_score : score;
            } score += 175;
            if(score < beta && depth <= 2) {
                new_score = quiescence(alpha, beta);
                if(new_score < beta) { return new_score > score ? new_score : score; }
            }
        }
	}

	if(in_pv) {
		in_pv = false;
		for(int i = 0; i < move_group.count; i++) { if(pv_table[0][ply] == move_group.moves[i]) { in_pv = true; break; } }
	} order_moves(&move_group, best_move);
	for(int cnt = 0; cnt < move_group.count; cnt++) {
		const bool capture = decode_cpt_flg(move_group.moves[cnt]);
		copy_board_state(); ply++;
		make_move(move_group.moves[cnt]);
		repetition_table[repetition_index] = pos_hash; repetition_index++;
		
        if(!cnt) { score = -negamax(-beta, -alpha, depth - 1); }
        else {
            if(cnt > 3 && depth > 2 && !in_check && !capture && !decode_pmt_flg(move_group.moves[cnt])) {
                score = -negamax(-alpha - 1, -alpha, depth - 2);
			} else { score = alpha + 1; }
            if(score > alpha) {
                score = -negamax(-alpha - 1, -alpha, depth - 1);
                if((score > alpha) && (score < beta)) { score = -negamax(-beta, -alpha, depth - 1); }
            }
        }
		
		revert_board_state(); ply--; repetition_index--;

		if(score >= beta) {
			write_hash_entry(beta, best_move, depth, hash_flag_beta);
			if(!capture) {
				killer_moves[1][ply] = killer_moves[0][ply];
				killer_moves[0][ply] = move_group.moves[cnt];
			} return beta;
		} if(score > alpha) {
			hash_flag = hash_flag_exact;
			best_move = move_group.moves[cnt];
			if(!capture) { history_moves[decode_piece(move_group.moves[cnt])][decode_trgtsq(move_group.moves[cnt])] += depth; }
			alpha = score;
			pv_table[ply][ply] = move_group.moves[cnt];
			for(int nply = ply + 1; nply < pv_length[ply + 1]; nply++) { pv_table[ply][nply] = pv_table[ply + 1][nply]; }
			pv_length[ply] = pv_length[ply + 1];	
		}
	}

	write_hash_entry(alpha, best_move, depth, hash_flag);
	return alpha;
}

inline void search(int depth) {
	int start_time = get_time(); int score = 0; nodes = 0; int best_move = 0;
	memset(killer_moves, 0, sizeof(killer_moves)); memset(history_moves, 0, sizeof(history_moves));
	memset(pv_table, 0, sizeof(pv_table)); memset(pv_length, 0, sizeof(pv_length));
	int alpha = -50000, beta = 50000;
	for(int current_depth = 1; current_depth <= depth; current_depth++) {
		in_pv = true;
		score = negamax(alpha, beta, current_depth);
		if((score <= alpha) || (score >= beta)) { alpha = -50000; beta = 50000; current_depth--; continue; }
        alpha = score - 40; beta = score + 40;
		if(stop_flag) { break; }
		best_move = pv_table[0][0];
		cout << "info score cp " << score << " depth " << current_depth << " nodes " << nodes << " time " << (get_time() - start_time) << " pv ";
		for(int i = 0; i < pv_length[0]; i++) { print_move(pv_table[0][i]); cout << " "; } cout << endl;
	} cout << "bestmove "; print_move(best_move); cout << endl;
}

/**********************************************************************************\
 *
 *                             Perft Functions
 *
\**********************************************************************************/

inline void perft_driver(int depth) {
	if (depth == 0) { nodes++; return; }
	MoveGroup move_group;
	generate_moves(&move_group, 1);
	for (int move_count = 0; move_count < move_group.count; move_count++) {
		copy_board_state();
		make_move(move_group.moves[move_count]);
		perft_driver(depth - 1);
		revert_board_state();
	}
}

void perft_test(int depth) {
	string promoted_pieces[] = {"", "n", "b", "r", "q", "", "", "n", "b", "r", "q"};
	nodes = 0;
	MoveGroup move_group;
	generate_moves(&move_group, 1);
	int start = get_time();
	for (int move_count = 0; move_count < move_group.count; move_count++) {
		copy_board_state();
		make_move(move_group.moves[move_count]);
		long old_nodes = nodes;
		perft_driver(depth - 1);
		long move_nodes = nodes - old_nodes;
		revert_board_state();
		cout << idx_to_name[decode_homesq(move_group.moves[move_count])];
		cout << idx_to_name[decode_trgtsq(move_group.moves[move_count])];
		cout << promoted_pieces[decode_pmt_flg(move_group.moves[move_count])];
		cout << "   nodes: " << move_nodes << endl;
	} int time_elapsed = get_time();
	cout << endl << "Depth: " << depth << endl;
	cout << "Nodes: " << nodes << endl;
	cout << " Time: " << time_elapsed - start << "ms" << endl;
	cout << "  NPS: " << (nodes / (time_elapsed - start)) * 1000 << " nps" << endl;
}

void coding_adventure_full_suite() {
	cout << endl << "Running Full Test Suite:" << endl << endl;
	int total_nodes = 0; int total_ms = 0;
	string position_list[30] = {
		"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
		"2b1b3/1r1P4/3K3p/1p6/2p5/6k1/1P3p2/4B3 w - - 0 42",
		"8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -",
		"r3k2r/pp3pp1/PN1pr1p1/4p1P1/4P3/3P4/P1P2PP1/R3K2R w KQkq - 4 4",
		"rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8",
		"r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
		"r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -",
		"r3k1nr/p2pp1pp/b1n1P1P1/1BK1Pp1q/8/8/2PP1PPP/6N1 w kq - 0 1",
		"3k4/3p4/8/K1P4r/8/8/8/8 b - - 0 1",
		"8/8/1k6/2b5/2pP4/8/5K2/8 b - d3 0 1",
		"5k2/8/8/8/8/8/8/4K2R w K - 0 1",
		"3k4/8/8/8/8/8/8/R3K3 w Q - 0 1",
		"r3k2r/1b4bq/8/8/8/8/7B/R3K2R w KQkq - 0 1",
		"r3k2r/8/3Q4/8/8/5q2/8/R3K2R b KQkq - 0 1",
		"2K2r2/4P3/8/8/8/8/8/3k4 w - - 0 1",
		"8/8/1P2K3/8/2n5/1q6/8/5k2 b - - 0 1",
		"4k3/1P6/8/8/8/8/K7/8 w - - 0 1",
		"8/P1k5/K7/8/8/8/8/8 w - - 0 1",
		"K1k5/8/P7/8/8/8/8/8 w - - 0 1",
		"8/k1P5/8/1K6/8/8/8/8 w - - 0 1",
		"8/8/2k5/5q2/5n2/8/5K2/8 b - - 0 1",
		"r1bq2r1/1pppkppp/1b3n2/pP1PP3/2n5/2P5/P3QPPP/RNB1K2R w KQ a6 0 12",
		"r3k2r/pppqbppp/3p1n1B/1N2p3/1nB1P3/3P3b/PPPQNPPP/R3K2R w KQkq - 11 10",
		"4k2r/1pp1n2p/6N1/1K1P2r1/4P3/P5P1/1Pp4P/R7 w k - 0 6",
		"1Bb3BN/R2Pk2r/1Q5B/4q2R/2bN4/4Q1BK/1p6/1bq1R1rb w - - 0 1",
		"n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1",
		"8/PPPk4/8/8/8/8/4Kppp/8 b - - 0 1",
		"8/2k1p3/3pP3/3P2K1/8/8/8/8 w - - 0 1",
		"3r4/2p1p3/8/1P1P1P2/3K4/5k2/8/8 b - - 0 1",
		"8/1p4p1/8/q1PK1P1r/3p1k2/8/4P3/4Q3 b - - 0 1"
	}; int correct_counts[30] = { 4865609, 5617302, 11030083, 15587335, 89941194, 3894594, 193690690, 497787, 1134888, 1440467, 661072, 15594314, 1274206, 58773923, 3821001, 1004658, 217342, 92683,5966690, 567584, 3114998, 42761834, 3050662, 10574719, 6871272, 71179139, 28859283, 7618365, 28181, 6323457 };
	int depths[30] = {5,5,6,5,5,4,5,4,6,6,6,7,4,5,6,5,6,6,10,7,6,5,4,5,4,6,6,9,4,5};
	for (int i = 0; i < 30; i++) {
		parse_fen(position_list[i]);
		int depth = depths[i];
		nodes = 0;
		MoveGroup move_group;
		generate_moves(&move_group, 1);
		int start = get_time();
		for (int move_count = 0; move_count < move_group.count; move_count++) {
			copy_board_state();
			make_move(move_group.moves[move_count]);
			perft_driver(depth - 1);
			revert_board_state();
		} int pos_time = get_time() - start;
		total_nodes += nodes;
		total_ms += pos_time;
		bool pass = correct_counts[i] == nodes;
		cout << left << setw(14) << setfill(' ') << "Test " + to_string(i + 1) + ": ";
		cout << left << setw(22) << setfill(' ') << "Nodes: " + to_string(nodes);
		cout << left << setw(14) << setfill(' ') << to_string(pos_time) + " ms";
		HANDLE hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
		SetConsoleTextAttribute(hConsole, (pass ? 10 : 12));
		cout << left << setw(6) << setfill(' ') << (pass ? "Pass" : "Fail") << endl;
		SetConsoleTextAttribute(hConsole, 7); 
	} cout << endl << "Total Time: " << total_ms << "ms";
	cout << "   avg nps: " << (total_nodes / total_ms) * 1000 << endl;
}

/**********************************************************************************\
 *
 *                             UCI Protocol
 *
\**********************************************************************************/

int parse_move(const string str) {
	MoveGroup move_group;
	generate_moves(&move_group, 1);
	int homesq = (str[0] - 'a') + (8 - (str[1] - '0')) * 8;
	int trgtsq = (str[2] - 'a') + (8 - (str[3] - '0')) * 8;
	for (int move_cnt = 0; move_cnt < move_group.count; move_cnt++) {
		int move = move_group.moves[move_cnt];
		if (homesq == decode_homesq(move) && trgtsq == decode_trgtsq(move)) {
			int promotion_flag = decode_pmt_flg(move);
			if (promotion_flag) {
				char promotion_char = tolower(str[4]);
				if ((promotion_flag == Q || promotion_flag == q) && promotion_char == 'q') { return move; }
				else if ((promotion_flag == R || promotion_flag == r) && promotion_char == 'r') { return move; }
				else if ((promotion_flag == B || promotion_flag == b) && promotion_char == 'b') { return move; }
				else if ((promotion_flag == N || promotion_flag == n) && promotion_char == 'n') { return move; }
				continue;
			} return move;
		}
	} return 0;
}

void handle_position_command(string str) {
	char command[str.length() + 1];
	strcpy(command, str.c_str());
	char* ptr = command;
	ptr = strstr(command, "startpos");
	if (ptr) { parse_fen(start_position); }
	ptr = strstr(command, "kiwi");
	if (ptr) { parse_fen(kiwi); }
	ptr = strstr(command, "fen");
	if (ptr) {
		ptr += 4;
		if (*ptr == '\0') { parse_fen(start_position); }
		else { parse_fen(ptr); }
	} ptr = strstr(command, "moves");
	if (ptr != NULL) {
		ptr += 6;
		while (*ptr) {
			int move = parse_move(ptr);
			if(move!=0) {
				make_move(move);
				repetition_table[repetition_index] = pos_hash;
				repetition_index++;
			} while (*ptr && *ptr != ' ') { ptr++; }
			ptr++;
		}
	}
}

void handle_go_command(const string str) {
    int movestogo = 5; // todo movestogo calculation based on occupancy count
    int movetime = 0, sidetime = 0, inc = 0, depth = 63;
	stop_flag = 0; stop_time = get_time() + 3000;

    char command[str.length() + 1];
    strcpy(command, str.c_str());
    char* ptr = command;

	ptr = strstr(command, "depth");
	if(ptr) { depth = atoi(ptr + 5); stop_time = get_time() + 60000; }
	ptr = strstr(command, "perft");
	if(ptr) { perft_test(atoi(ptr + 5)); return; }
	ptr = strstr(command, "movetime");
	if(ptr) { movetime = atoi(ptr + 8); }
	ptr = strstr(command, "movestogo");
	if(ptr) { movestogo = atoi(ptr + 9); }
	ptr = strstr(command, "wtime");
	if(ptr && !color) { sidetime = atoi(ptr + 5); }
	ptr = strstr(command, "btime");
	if(ptr && color) { sidetime = atoi(ptr + 5); }
	ptr = strstr(command, "winc");
	if(ptr && !color) { inc = atoi(ptr + 4); }
	ptr = strstr(command, "binc");
	if(ptr && color) { inc = atoi(ptr + 4); }

	if (movetime) { stop_time = get_time() + movetime + inc; }
	else if(sidetime) { stop_time = get_time() + (sidetime / movestogo) + inc; }

    search(depth);
}

int uci() {
	string command;
	cout.setf(ios::unitbuf);
	while (getline(cin, command)) {
		if (command == "\n") { continue; }
		else if (command == "uci") {
			cout << "id name Norbit" << endl;
			cout << "id author Nick" << endl << "uciok" << endl;
		} else if (command == "isready") {
			cout << "readyok" << endl;
		} else if (command == "ucinewgame") {
			clear_hash_table();
			parse_fen(start_position);
		} else if (command.substr(0, 8) == "position") {
			clear_hash_table();
			handle_position_command(command);
		} else if (command.substr(0, 2) == "go") {
			handle_go_command(command);
		} else if (command == "quit") {
			cout << "Quit" << endl; break;
		} else if (command == "rfs") {
			coding_adventure_full_suite();
		} else if (command == "print") {
			print_board();
		}
	} return 0;
}

/**********************************************************************************\
 *
 *                             Main Driver
 *
\**********************************************************************************/

int main() {
	init_hash_table(512);
	parse_fen(start_position);

	uci();

	free(hash_table);
	return 0;
}