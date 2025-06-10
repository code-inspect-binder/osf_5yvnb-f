#define EXP_C

#include "trial_exp6.c"
#include "config_serialport.c"

//////////////////////////////////////////////////////////////////
///////////				DEFINITIONS			//////////////////////
/////////////////////////////////////////////////////////////////

struct{	
	int resp_enter;
	int resp_wijs;
}instr;


int ppninfo()
{

//PARAMETERS

	ts5_set_foreground_color(ts5_make_named_color ("white", 1.0));	
	ts5_set_coordinate_system(TS5_DISPLAY_COORDINATES);
	ts5_set_text_alignment(TS5_ALIGN_LEFT);

//DEFINITIONS

	int i;
	int ok;
	int code;
	int ppn;
	int lftd;
	char mv;	

	double x = 10.0, y = 10.0, w;
	
//TEST OR EXPERIMENT
	
	do{
		w = ts5_printf(x, y, "test = 1, exp = 2: ");
		ts5_flip_display();
		ok = ts5_scanf(x+w, y, "%d", &code);
		ts5_clear_display();
		ts5_flip_display();
	}while(!ok || code < 1 || code > 2);
	
	if(code == 1){
		ts5_define_keyboard_button(-TS5_KEY_S);
		ts5_define_keyboard_button(-TS5_KEY_L);	
	}else{
		int portnum = ts5_define_serialport_response_input("COM6");
		TS5_SERIALPORT *portptr = ts5_get_serialport(portnum);
		config_serialport(portptr);
	
		ts5_define_serialport_button(1, 50);
		ts5_define_serialport_button(1, 52);		
	}
	
	ts5_define_keyboard_button(TS5_KEY_ESCAPE);
	ts5_define_keyboard_button(TS5_KEY_ENTER);
	
//PPN NUMMER
	
	do{
		w = ts5_printf(x, y, "ppn: ");
		ts5_flip_display();
		ok = ts5_scanf(x+w, y, "%d", &ppn);
		ts5_clear_display();
		ts5_flip_display();
	}while(!ok || ppn < 0);
	
//LFTD
	
	do{
		w = ts5_printf(x, y, "leeftijd: ");
		ts5_flip_display();
		ok = ts5_scanf(x+w, y, "%d", &lftd);
		ts5_clear_display();
		ts5_flip_display();
	}while(!ok || lftd <= 0);
	
//GESL
	
	do{
		w = ts5_printf(x, y, "geslacht (m/v): ");
		ts5_flip_display();
		ok = ts5_scanf(x+w, y, "%c", &mv);
		ts5_clear_display();
		ts5_flip_display();
	}while(!ok || (mv != 'm' && mv != 'v'));
	
//PARAMETERS

	ts5_set_coordinate_system(TS5_CARTESIAN_COORDINATES);
	ts5_set_text_alignment(TS5_ALIGN_CENTER);
	
//WEGSCHRIJVEN
	
	for(i = 0; i < NTRIALS; i++){
		data[i].ppn = ppn;		
		data[i].lftd = lftd;		
		data[i].mv = mv;
	}
	
	return ppn;
}

void instructies_deel1()
{

//PARAMETERS

	ts5_set_foreground_color (ts5_make_named_color ("white", 1.0));	
	ts5_set_font_type(TS5_ARIAL);
    ts5_set_font_style(TS5_REGULAR);
	ts5_set_font_size(20);
	ts5_set_text_alignment(TS5_ALIGN_CENTER);
	
//INSTRUCTIES DEEL 1

	//instructies
	
	ts5_printf(0, ay(0.6), "Welkom!");
	
	ts5_printf(0, ay(0.4), "In dit experiment zal je ofwel de letter W ofwel de letter P in het midden van het scherm zien");
	ts5_printf(0, ay(0.3), "Wanneer je de letter W ('wijsvinger') ziet, moet je je wijsvinger opzij bewegen");
	ts5_printf(0, ay(0.2), "Wanneer je de letter P ('pink') ziet, moet je je pink opzij bewegen");	
	
	ts5_printf(0, ay(0), "Tegelijk zal je 4 handen op het scherm zien waarvan 0, 1, 2, 3, of 4 handen een beweging zullen maken");	
	ts5_printf(0, ay(-0.1), "Wanneer de handen bewegen, zullen je helpen bij het uitvoeren van je taak");	
	ts5_printf(0, ay(-0.2), "Ze zullen dan namelijk altijd de actie uitvoeren die jij ook moet uitvoeren");
	ts5_printf(0, ay(-0.3), "Wanneer de letter W verschijnt zullen ze de wijsvinger bewegen");
	ts5_printf(0, ay(-0.4), "Wanneer de letter P verschijnt zullen ze de pink bewegen");
				
	ts5_printf(0, ay(-0.6), "Druk op ENTER om een voorbeeld te zien");
	
	ts5_flip_display();
			
	//wachten op respons
	
	do{
		ts5_flush_responses();
		instr.resp_enter = ts5_wait_for_response(&tmp.junk, &tmp.junk);
	}while(instr.resp_enter != 4);
	
	ts5_clear_display();
	ts5_flip_display();
	
//VOORBEELD

	//tonen
	
	TS5_BITMAP *map;	
		
	ts5_printf(0, ay(0.97), "Reageer op de letter om verder te gaan");
		
	map = ts5_read_bitmap("Stimuli/W/3mov/W_3mov_pos1.png");
	ts5_draw_scaled_bitmap(map, 1.5, 1.5, 0, 0);
	
	ts5_set_font_type(TS5_ARIAL);
	ts5_set_font_style(TS5_BOLD);
	ts5_set_font_size(26);
	ts5_printf(0,0,"W");

	ts5_flip_display();	

	//wachten op respons
	
	do{
		ts5_flush_responses();
		instr.resp_enter = ts5_wait_for_response(&tmp.junk, &tmp.junk);
	}while(instr.resp_enter != 1);
	
	ts5_clear_display();
	ts5_flip_display();	
	
//INSTRUCTIES DEEL 2

	//parameters herinstellen

	ts5_set_foreground_color (ts5_make_named_color ("white", 1.0));	
	ts5_set_font_type(TS5_ARIAL);
    ts5_set_font_style(TS5_REGULAR);
	ts5_set_font_size(20);

	//instructies
		
	ts5_printf(0, ay(0.2), "Dit is een reactietijd experiment. Probeer dus om zo snel mogelijk te reageren, maar probeer tegelijk ook fouten te vermijden.");
	ts5_printf(0, ay(0.1), "Het experiment begint met een korte oefenfase waarin je feedback krijgt op je prestatie");
				
	ts5_printf(0, ay(-0.1), "Druk op ENTER om te beginnen");
	
	ts5_flip_display();
			
	//wachten op respons
	
	do{
		ts5_flush_responses();
		instr.resp_enter = ts5_wait_for_response(&tmp.junk, &tmp.junk);
	}while(instr.resp_enter != 4);
	
	ts5_clear_display();
	ts5_flip_display();
	
//AFTELLEN

	int i;

	for(i = 3; i > 0; i--){
		ts5_printf(0, ay(0), "%d", i);
		ts5_flip_display();
		ts5_wait(1);
		ts5_clear_display();
		ts5_flip_display();
	}	
	
}

void instructies_deel2()
{

//PARAMETERS

	ts5_set_foreground_color (ts5_make_named_color ("white", 1.0));	
	ts5_set_font_type(TS5_ARIAL);
    ts5_set_font_style(TS5_REGULAR);
	ts5_set_font_size(20);
	ts5_set_text_alignment(TS5_ALIGN_CENTER);

//INSTRUCTIES

	//instructies

	ts5_printf(0, ay(0.6), "Nu start het eigenlijke experiment");
	ts5_printf(0, ay(0.5), "De taak is dezelfde als in de oefenfase, alleen krijg je nu geen feedback meer");
		    
	ts5_printf(0, ay(0.3), "Beweeg je wijsvinger opzij als je de W ('wijsvinger') ziet");
	ts5_printf(0, ay(0.2), "Beweeg je pink opzij als je de P ('pink') ziet");
	ts5_printf(0, ay(0.1), "Wanneer de handen op het scherm bewegen, zullen je helpen bij je taak");
	
	ts5_printf(0, ay(0), "Onthoud dat dit een reactietijdexperiment is");
	ts5_printf(0, ay(-0.1), "Probeer dus om zo snel mogelijk te reageren, maar probeer tegelijk ook fouten te vermijden");	
	
	ts5_printf(0, ay(-0.3), "Indien je nog vragen hebt of indien iets niet duidelijk is, geef dan een seintje aan de proefleider");
	ts5_printf(0, ay(-0.4), "Als alles duidelijk is, druk dan op ENTER om aan het experiment te beginnen");
	
	ts5_flip_display();
	
	//wachten op respons
	
	do{
		ts5_flush_responses();
		instr.resp_enter = ts5_wait_for_response(&tmp.junk, &tmp.junk);
	}while(instr.resp_enter != 4);
	
	ts5_clear_display();
	ts5_flip_display();
	
//AFTELLEN

	int i;

	for(i = 3; i > 0; i--){
		ts5_printf(0, ay(0), "%d", i);
		ts5_flip_display();
		ts5_wait(1);
		ts5_clear_display();
		ts5_flip_display();
	}
}

void pauze()
{

//PARAMETERS

	ts5_set_foreground_color (ts5_make_named_color ("white", 1.0));	
	ts5_set_font_type(TS5_ARIAL);
    ts5_set_font_style(TS5_REGULAR);
	ts5_set_font_size(20);
	ts5_set_text_alignment(TS5_ALIGN_CENTER);
	
//TEKST
	
	//tekst

	ts5_printf(0, ay(0.1), "Je kan nu even pauzeren");
	ts5_printf(0, ay(-0.1), "Druk op ENTER om verder te gaan");
	
	ts5_flip_display();
	
	//wachten op respons
	
	do{
		ts5_flush_responses();
		instr.resp_enter = ts5_wait_for_response(&tmp.junk, &tmp.junk);
	}while(instr.resp_enter != 4);
	
	ts5_clear_display();
	ts5_flip_display();
	
//AFTELLEN

	int i;

	for(i = 3; i > 0; i--){
		ts5_printf(0, ay(0), "%d", i);
		ts5_flip_display();
		ts5_wait(1);
		ts5_clear_display();
		ts5_flip_display();
	}

}

void bedanking()
{

//PARAMETERS

	ts5_set_foreground_color (ts5_make_named_color ("white", 1.0));	
	ts5_set_font_type(TS5_ARIAL);
    ts5_set_font_style(TS5_REGULAR);
	ts5_set_font_size(20);
	ts5_set_text_alignment(TS5_ALIGN_CENTER);
	
//TEKST
	
	//tekst

	ts5_printf(0, ay(0), "Bedankt voor je deelname!");
	
	ts5_flip_display();
	
	//wachten op respons
	
	do{
		ts5_flush_responses();
		instr.resp_enter = ts5_wait_for_response(&tmp.junk, &tmp.junk);
	}while(instr.resp_enter != 4);
	
	ts5_clear_display();
	ts5_flip_display();
}

int main()
{

//SCREEN

	ts5_set_display_mode(TS5_FULLSCREEN_WINDOW);
	ts5_set_vsync_mode(TS5_VSYNC_ON);
	ts5_install_display();	
	ts5_hide_mouse();
	
//EXPERIMENT
	
	//definties
	
	int i;
	
	//initialiseren
	
	initialiseer();
	
	//subject info
	
	int ppn = ppninfo();
    
	//randomisatie
	
	randomisatie_oefen();
	randomisatie(ppn);
  
	//oefenfase
	
	instructies_deel1();
	
	for(i = 0; i < NTRIALS_OEFEN; i++){
		trial_oefen(i);		
	}	
	
	//experiment
	
	instructies_deel2();
	
	for(i = 0; i < NTRIALS; i++){
		if(i == (NTRIALS) / 4 || i == (2 * NTRIALS) / 4 || i == (3 * NTRIALS) / 4){
			pauze();
		}
		trial(i);		
	}
		
	//wegschrijven
	
	schrijf();
		
	//bedanking
	
	bedanking();	

	
	return 0;	
}
