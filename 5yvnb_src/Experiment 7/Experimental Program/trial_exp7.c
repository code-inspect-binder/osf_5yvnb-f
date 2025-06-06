#define TRIAL_C

#include "rnd_exp7.c"

//////////////////////////////////////////////////////////////////
///////////				DEFINITIONS			//////////////////////
/////////////////////////////////////////////////////////////////

#define FIXTM	0.5
#define STMTM 2
#define ITI	1

struct{
	double junk;
	double t1; 
	double t2, e2;
}tmp;

//////////////////////////////////////////////////////////////////
///////////				TRIAL OEFEN			//////////////////////
/////////////////////////////////////////////////////////////////

void trial_oefen(int i)
{

//SCREEN PARAMETERS
	
	ts5_set_foreground_color(ts5_make_named_color("black", 1.0));
	ts5_set_font_type(TS5_ARIAL);
	ts5_set_font_style(TS5_BOLD);
	ts5_set_font_size(26);
	
//FIXATIE

	//neutrale prent

	TS5_BITMAP *map;
		
	if(oefen[i].vinger == wijs_oefen){	
		map = ts5_read_bitmap("Stimuli/W/neutr/W_neutr.png");		
	}else{
		map = ts5_read_bitmap("Stimuli/P/neutr/P_neutr.png");		
	}
	
	ts5_draw_scaled_bitmap(map, 1.5, 1.5, 0, 0);
	
	//fixatiekruis
	
	ts5_printf(0,0, "+");
	
	//printen	
	
	ts5_flip_display();
	
	//wachten
	
	ts5_wait(FIXTM);
		
//PICTURE MOVEMENT

	//prent
	
	if(oefen[i].aantal == 1){
		if(oefen[i].vinger == wijs_oefen){
			if(oefen[i].pos == 1){
				map = ts5_read_bitmap("Stimuli/W/1mov/W_1mov_pos1.png");
			}else if(oefen[i].pos == 2){
				map = ts5_read_bitmap("Stimuli/W/1mov/W_1mov_pos2.png");
			}else if(oefen[i].pos == 3){
				map = ts5_read_bitmap("Stimuli/W/1mov/W_1mov_pos3.png");
			}else if(oefen[i].pos == 4){
				map = ts5_read_bitmap("Stimuli/W/1mov/W_1mov_pos4.png");
			}		
		}else{
			if(oefen[i].pos == 1){
				map = ts5_read_bitmap("Stimuli/P/1mov/P_1mov_pos1.png");
			}else if(oefen[i].pos == 2){
				map = ts5_read_bitmap("Stimuli/P/1mov/P_1mov_pos2.png");
			}else if(oefen[i].pos == 3){
				map = ts5_read_bitmap("Stimuli/P/1mov/P_1mov_pos3.png");
			}else if(oefen[i].pos == 4){
				map = ts5_read_bitmap("Stimuli/P/1mov/P_1mov_pos4.png");
			}			
		}
	}else if(oefen[i].aantal == 2){
		if(oefen[i].vinger == wijs_oefen){
			if(oefen[i].pos == 1){
				map = ts5_read_bitmap("Stimuli/W/2mov/W_2mov_pos1.png");
			}else if(oefen[i].pos == 2){
				map = ts5_read_bitmap("Stimuli/W/2mov/W_2mov_pos2.png");
			}else if(oefen[i].pos == 3){
				map = ts5_read_bitmap("Stimuli/W/2mov/W_2mov_pos3.png");
			}else if(oefen[i].pos == 4){
				map = ts5_read_bitmap("Stimuli/W/2mov/W_2mov_pos4.png");
			}else if(oefen[i].pos == 5){
				map = ts5_read_bitmap("Stimuli/W/2mov/W_2mov_pos5.png");
			}else if(oefen[i].pos == 6){
				map = ts5_read_bitmap("Stimuli/W/2mov/W_2mov_pos6.png");
			}
		}else{
			if(oefen[i].pos == 1){
				map = ts5_read_bitmap("Stimuli/P/2mov/P_2mov_pos1.png");
			}else if(oefen[i].pos == 2){
				map = ts5_read_bitmap("Stimuli/P/2mov/P_2mov_pos2.png");
			}else if(oefen[i].pos == 3){
				map = ts5_read_bitmap("Stimuli/P/2mov/P_2mov_pos3.png");
			}else if(oefen[i].pos == 4){
				map = ts5_read_bitmap("Stimuli/P/2mov/P_2mov_pos4.png");
			}else if(oefen[i].pos == 5){
				map = ts5_read_bitmap("Stimuli/P/2mov/P_2mov_pos5.png");
			}else if(oefen[i].pos == 6){
				map = ts5_read_bitmap("Stimuli/P/2mov/P_2mov_pos6.png");
			}		
		}	
	}else if(oefen[i].aantal == 3){
		if(oefen[i].vinger == wijs_oefen){
			if(oefen[i].pos == 1){
				map = ts5_read_bitmap("Stimuli/W/3mov/W_3mov_pos1.png");
			}else if(oefen[i].pos == 2){
				map = ts5_read_bitmap("Stimuli/W/3mov/W_3mov_pos2.png");
			}else if(oefen[i].pos == 3){
				map = ts5_read_bitmap("Stimuli/W/3mov/W_3mov_pos3.png");
			}else if(oefen[i].pos == 4){
				map = ts5_read_bitmap("Stimuli/W/3mov/W_3mov_pos4.png");
			}		
		}else{
			if(oefen[i].pos == 1){
				map = ts5_read_bitmap("Stimuli/P/3mov/P_3mov_pos1.png");
			}else if(oefen[i].pos == 2){
				map = ts5_read_bitmap("Stimuli/P/3mov/P_3mov_pos2.png");
			}else if(oefen[i].pos == 3){
				map = ts5_read_bitmap("Stimuli/P/3mov/P_3mov_pos3.png");
			}else if(oefen[i].pos == 4){
				map = ts5_read_bitmap("Stimuli/P/3mov/P_3mov_pos4.png");
			}			
		}	
	}else if(oefen[i].aantal == 4){
		if(oefen[i].vinger == wijs_oefen){
			map = ts5_read_bitmap("Stimuli/W/4mov/W_4mov.png");
		}else{
			map = ts5_read_bitmap("Stimuli/P/4mov/P_4mov.png");
		}	
	}
	
	ts5_draw_scaled_bitmap(map, 1.5, 1.5, 0, 0);
	
	//stimulus
	
	ts5_printf(0,0,"X");

	//printen
	
	tmp.t1 = ts5_flip_display();
	
//RESPONSE

	//record response

	ts5_flush_responses();
	oefen[i].r = ts5_wait_for_response_timed(&tmp.t2, &tmp.e2, STMTM);
	
	//clear screen
		
	ts5_clear_display();
	ts5_flip_display();
	
//BREAK IF ESCAPE

	if(oefen[i].r == 3){
		schrijf();
		ts5_fatal("experiment afgebroken");		
	}
	
//ACCURACY

	if(oefen[i].r == oefen[i].xr){
		oefen[i].corr = 1;
		ts5_set_foreground_color(ts5_make_named_color("green", 1.0));
		ts5_printf(0,0,"CORRECT");
	}else{
		oefen[i].corr = 0;
		ts5_set_foreground_color(ts5_make_named_color("red", 1.0));
		ts5_printf(0,0,"FOUT");
	}
	
	ts5_flip_display();
	
//ITI

	//wait

	ts5_wait(ITI);
	
	//clear screen
	
	ts5_clear_display();
	ts5_flip_display();	

//FREE BITMAP

	ts5_free_bitmap(map);
}

//////////////////////////////////////////////////////////////////
///////////				TRIAL EXP			//////////////////////
/////////////////////////////////////////////////////////////////

void trial(int i)
{

//SCREEN PARAMETERS
	
	ts5_set_foreground_color(ts5_make_named_color("black", 1.0));
	ts5_set_font_type(TS5_ARIAL);
	ts5_set_font_style(TS5_BOLD);
	ts5_set_font_size(26);
	
//FIXATIE

	//neutrale prent

	TS5_BITMAP *map;
		
	if(data[i].vinger == wijs){	
		map = ts5_read_bitmap("Stimuli/W/neutr/W_neutr.png");		
	}else{
		map = ts5_read_bitmap("Stimuli/P/neutr/P_neutr.png");		
	}
	
	ts5_draw_scaled_bitmap(map, 1.5, 1.5, 0, 0);
	
	//fixatiekruis
	
	ts5_printf(0,0, "+");
	
	//printen	
	
	ts5_flip_display();
	
	//wachten
	
	ts5_wait(FIXTM);
	
//PICTURE MOVEMENT

	//prent
	
	if(data[i].aantal == 1){
		if(data[i].vinger == wijs){
			if(data[i].pos == 1){
				map = ts5_read_bitmap("Stimuli/W/1mov/W_1mov_pos1.png");
			}else if(data[i].pos == 2){
				map = ts5_read_bitmap("Stimuli/W/1mov/W_1mov_pos2.png");
			}else if(data[i].pos == 3){
				map = ts5_read_bitmap("Stimuli/W/1mov/W_1mov_pos3.png");
			}else if(data[i].pos == 4){
				map = ts5_read_bitmap("Stimuli/W/1mov/W_1mov_pos4.png");
			}		
		}else{
			if(data[i].pos == 1){
				map = ts5_read_bitmap("Stimuli/P/1mov/P_1mov_pos1.png");
			}else if(data[i].pos == 2){
				map = ts5_read_bitmap("Stimuli/P/1mov/P_1mov_pos2.png");
			}else if(data[i].pos == 3){
				map = ts5_read_bitmap("Stimuli/P/1mov/P_1mov_pos3.png");
			}else if(data[i].pos == 4){
				map = ts5_read_bitmap("Stimuli/P/1mov/P_1mov_pos4.png");
			}			
		}
	}else if(data[i].aantal == 2){
		if(data[i].vinger == wijs){
			if(data[i].pos == 1){
				map = ts5_read_bitmap("Stimuli/W/2mov/W_2mov_pos1.png");
			}else if(data[i].pos == 2){
				map = ts5_read_bitmap("Stimuli/W/2mov/W_2mov_pos2.png");
			}else if(data[i].pos == 3){
				map = ts5_read_bitmap("Stimuli/W/2mov/W_2mov_pos3.png");
			}else if(data[i].pos == 4){
				map = ts5_read_bitmap("Stimuli/W/2mov/W_2mov_pos4.png");
			}else if(data[i].pos == 5){
				map = ts5_read_bitmap("Stimuli/W/2mov/W_2mov_pos5.png");
			}else if(data[i].pos == 6){
				map = ts5_read_bitmap("Stimuli/W/2mov/W_2mov_pos6.png");
			}
		}else{
			if(data[i].pos == 1){
				map = ts5_read_bitmap("Stimuli/P/2mov/P_2mov_pos1.png");
			}else if(data[i].pos == 2){
				map = ts5_read_bitmap("Stimuli/P/2mov/P_2mov_pos2.png");
			}else if(data[i].pos == 3){
				map = ts5_read_bitmap("Stimuli/P/2mov/P_2mov_pos3.png");
			}else if(data[i].pos == 4){
				map = ts5_read_bitmap("Stimuli/P/2mov/P_2mov_pos4.png");
			}else if(data[i].pos == 5){
				map = ts5_read_bitmap("Stimuli/P/2mov/P_2mov_pos5.png");
			}else if(data[i].pos == 6){
				map = ts5_read_bitmap("Stimuli/P/2mov/P_2mov_pos6.png");
			}		
		}	
	}else if(data[i].aantal == 3){
		if(data[i].vinger == wijs){
			if(data[i].pos == 1){
				map = ts5_read_bitmap("Stimuli/W/3mov/W_3mov_pos1.png");
			}else if(data[i].pos == 2){
				map = ts5_read_bitmap("Stimuli/W/3mov/W_3mov_pos2.png");
			}else if(data[i].pos == 3){
				map = ts5_read_bitmap("Stimuli/W/3mov/W_3mov_pos3.png");
			}else if(data[i].pos == 4){
				map = ts5_read_bitmap("Stimuli/W/3mov/W_3mov_pos4.png");
			}		
		}else{
			if(data[i].pos == 1){
				map = ts5_read_bitmap("Stimuli/P/3mov/P_3mov_pos1.png");
			}else if(data[i].pos == 2){
				map = ts5_read_bitmap("Stimuli/P/3mov/P_3mov_pos2.png");
			}else if(data[i].pos == 3){
				map = ts5_read_bitmap("Stimuli/P/3mov/P_3mov_pos3.png");
			}else if(data[i].pos == 4){
				map = ts5_read_bitmap("Stimuli/P/3mov/P_3mov_pos4.png");
			}			
		}	
	}else if(data[i].aantal == 4){
		if(data[i].vinger == wijs){
			map = ts5_read_bitmap("Stimuli/W/4mov/W_4mov.png");
		}else{
			map = ts5_read_bitmap("Stimuli/P/4mov/P_4mov.png");
		}	
	}
	
	ts5_draw_scaled_bitmap(map, 1.5, 1.5, 0, 0);
	
	//stimulus

	ts5_printf(0,0,"X");

	//printen
	
	tmp.t1 = ts5_flip_display();
	
//RESPONSE

	//record response

	ts5_flush_responses();
	data[i].r = ts5_wait_for_response_timed(&tmp.t2, &tmp.e2, STMTM);
	
	//clear screen
		
	ts5_clear_display();
	ts5_flip_display();
		
//RT + RE

	data[i].rt = (tmp.t2 - tmp.t1) * 1000;
	data[i].re = (tmp.e2) * 1000;
	
//BREAK IF ESCAPE

	if(data[i].r == 3){
		schrijf();
		ts5_fatal("experiment afgebroken");		
	}
	
//ACCURACY

	if(data[i].r == data[i].xr){
		data[i].corr = 1;		
	}else{
		data[i].corr = 0;
	}
	
//ITI

	ts5_wait(ITI);

//FREE BITMAP

	ts5_free_bitmap(map);	
}


#ifndef EXP_C
int main()
{

//INSTALL SCREEN
		
	ts5_set_display_mode(TS5_FULLSCREEN_WINDOW);
	ts5_set_vsync_mode(TS5_VSYNC_ON);
	ts5_install_display();
	ts5_hide_mouse();	

//RESPONSE BUTTONS

	ts5_define_keyboard_button(-TS5_KEY_S);
	ts5_define_keyboard_button(-TS5_KEY_L);
	ts5_define_keyboard_button(TS5_KEY_ESCAPE);
	
//EXPERIMENT
	
	int i;	
	int ppn = 1001;

	initialiseer();	
	randomisatie_oefen();
	randomisatie(ppn);
	
	for(i = 0; i < 5; i++){
		trial_oefen(i);
	}
    
	for(i = 0; i < 5; i++){
		trial(i);
	}

	schrijf();

	return 0;
}
#endif