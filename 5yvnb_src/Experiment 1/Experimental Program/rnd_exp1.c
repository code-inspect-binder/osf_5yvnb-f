#define RND_C

#include "data_exp1.c"

//////////////////////////////////////////////////////////////////
///////////				DEFINITIONS			//////////////////////
/////////////////////////////////////////////////////////////////

#define MAXREP  4
#define MAXIT	500000

//////////////////////////////////////////////////////////////////
///////////			RND PRACTICE			//////////////////////
/////////////////////////////////////////////////////////////////

void randomisatie_oefen()
{
	//declaratie variabelen

	int i;
	
	//creëeren lijsten
	
	int tmp[NTRIALS_OEFEN];
	
	//toewijzen lijsten
	
	ts5_random_list(NCELLS_OEFEN, NREPS_OEFEN, tmp);
	
	for(i = 0; i < NTRIALS_OEFEN; i++){
	
		//randomisatie
				
		oefen[i].cue = ( tmp[i] / (VINGER * AANTAL) ) + 1;
		oefen[i].vinger =  ( tmp[i] % (VINGER * AANTAL) / (AANTAL) ) + 1;
		oefen[i].aantal = ( tmp[i] % (VINGER * AANTAL) % (AANTAL) ) + 1;

		//positie random kiezen
		
		if(oefen[i].aantal == 1 || oefen[i].aantal == 3){
			oefen[i].pos = ts5_random_integer(4) + 1;
		}else if(oefen[i].aantal == 2){
			oefen[i].pos = ts5_random_integer(6) + 1;
		}
		
		//verwachte respons
		
		if(oefen[i].cue == W_oefen){
			oefen[i].xr = 1;	
		}else{
			oefen[i].xr = 2;			
		}
	}
}

//////////////////////////////////////////////////////////////////
///////////				RND EXP				//////////////////////
/////////////////////////////////////////////////////////////////

void randomisatie(int ppn)
{

	//declaratie variabelen

	int i;
	
	int stimrep;
	int ok, iteratie = 0;
	
	//creëeren lijsten
	
		//design
	
	int tmp[NTRIALS];
	
		//design pos
	
	int tmp_W_wijs_pos1[NTRIALS_POS];
	int tmp_W_pink_pos1[NTRIALS_POS];
	int tmp_P_wijs_pos1[NTRIALS_POS];
	int tmp_P_pink_pos1[NTRIALS_POS];
	
	int tmp_W_wijs_pos2[NTRIALS_POS];
	int tmp_W_pink_pos2[NTRIALS_POS];
	int tmp_P_wijs_pos2[NTRIALS_POS];
	int tmp_P_pink_pos2[NTRIALS_POS];
	
	int tmp_W_wijs_pos3[NTRIALS_POS];
	int tmp_W_pink_pos3[NTRIALS_POS];
	int tmp_P_wijs_pos3[NTRIALS_POS];
	int tmp_P_pink_pos3[NTRIALS_POS];
		
	do{
	
		//toewijzen lijsten
		
			//design
	
		for(i = 0; i < BLOK; i++){
			ts5_random_list(NCELLS, NREPS, &tmp[i * (NCELLS * NREPS)]);	
		}
		
			//design pos
		
		for(i = 0; i < BLOK_POS; i++){		
			ts5_random_list(NCELLS_POS1, NREPS_POS1, &tmp_W_wijs_pos1[i * (NCELLS_POS1 * NREPS_POS1)]);
			ts5_random_list(NCELLS_POS1, NREPS_POS1, &tmp_W_pink_pos1[i * (NCELLS_POS1 * NREPS_POS1)]);	
			ts5_random_list(NCELLS_POS1, NREPS_POS1, &tmp_P_wijs_pos1[i * (NCELLS_POS1 * NREPS_POS1)]);	
			ts5_random_list(NCELLS_POS1, NREPS_POS1, &tmp_P_pink_pos1[i * (NCELLS_POS1 * NREPS_POS1)]);	
			
			ts5_random_list(NCELLS_POS2, NREPS_POS2, &tmp_W_wijs_pos2[i * (NCELLS_POS2 * NREPS_POS2)]);
			ts5_random_list(NCELLS_POS2, NREPS_POS2, &tmp_W_pink_pos2[i * (NCELLS_POS2 * NREPS_POS2)]);
			ts5_random_list(NCELLS_POS2, NREPS_POS2, &tmp_P_wijs_pos2[i * (NCELLS_POS2 * NREPS_POS2)]);
			ts5_random_list(NCELLS_POS2, NREPS_POS2, &tmp_P_pink_pos2[i * (NCELLS_POS2 * NREPS_POS2)]);
			
			ts5_random_list(NCELLS_POS3, NREPS_POS3, &tmp_W_wijs_pos3[i * (NCELLS_POS3 * NREPS_POS3)]);
			ts5_random_list(NCELLS_POS3, NREPS_POS3, &tmp_W_pink_pos3[i * (NCELLS_POS3 * NREPS_POS3)]);
			ts5_random_list(NCELLS_POS3, NREPS_POS3, &tmp_P_wijs_pos3[i * (NCELLS_POS3 * NREPS_POS3)]);
			ts5_random_list(NCELLS_POS3, NREPS_POS3, &tmp_P_pink_pos3[i * (NCELLS_POS3 * NREPS_POS3)]);
		}
		
		//creëeren counters design pos
		
		int n_W_wijs_pos1 = 0;
		int n_W_pink_pos1 = 0;
		int n_P_wijs_pos1 = 0;
		int n_P_pink_pos1 = 0;
		
		int n_W_wijs_pos2 = 0;
		int n_W_pink_pos2 = 0;
		int n_P_wijs_pos2 = 0;
		int n_P_pink_pos2 = 0;
		
		int n_W_wijs_pos3 = 0;
		int n_W_pink_pos3 = 0;
		int n_P_wijs_pos3 = 0;
		int n_P_pink_pos3 = 0;
		
		for(i = 0; i < NTRIALS; i++){
		
			//ppnr

			data[i].ppn = ppn;
			
			//randomisatie
			
				//randomisatie
				
			data[i].cue = ( tmp[i] / (VINGER * AANTAL) ) + 1;
			data[i].vinger =  ( tmp[i] % (VINGER * AANTAL) / (AANTAL) ) + 1;
			data[i].aantal = ( tmp[i] % (VINGER * AANTAL) % (AANTAL) ) + 1;
			
				//blok
				
			data[i].blok = i / (NCELLS * NREPS);
			
				//positie verdelen
				
			if(data[i].aantal == 1){
				if(data[i].cue == W){
					if(data[i].vinger == wijs){
						data[i].pos = tmp_W_wijs_pos1[n_W_wijs_pos1] + 1;
						n_W_wijs_pos1++;
					}else{
						data[i].pos = tmp_W_pink_pos1[n_W_pink_pos1] + 1;
						n_W_pink_pos1++;					
					}
				}else{
					if(data[i].vinger == wijs){
						data[i].pos = tmp_P_wijs_pos1[n_P_wijs_pos1] + 1;
						n_P_wijs_pos1++;
					}else{
						data[i].pos = tmp_P_pink_pos1[n_P_pink_pos1] + 1;
						n_P_pink_pos1++;					
					}			
				}
			}else if(data[i].aantal == 2){
				if(data[i].cue == W){
					if(data[i].vinger == wijs){
						data[i].pos = tmp_W_wijs_pos2[n_W_wijs_pos2] + 1;
						n_W_wijs_pos2++;
					}else{
						data[i].pos = tmp_W_pink_pos2[n_W_pink_pos2] + 1;
						n_W_pink_pos2++;					
					}
				}else{
					if(data[i].vinger == wijs){
						data[i].pos = tmp_P_wijs_pos2[n_P_wijs_pos2] + 1;
						n_P_wijs_pos2++;
					}else{
						data[i].pos = tmp_P_pink_pos2[n_P_pink_pos2] + 1;
						n_P_pink_pos2++;					
					}			
				}			
			}else if(data[i].aantal == 3){
				if(data[i].cue == W){
					if(data[i].vinger == wijs){
						data[i].pos = tmp_W_wijs_pos3[n_W_wijs_pos3] + 1;
						n_W_wijs_pos3++;
					}else{
						data[i].pos = tmp_W_pink_pos3[n_W_pink_pos3] + 1;
						n_W_pink_pos3++;					
					}
				}else{
					if(data[i].vinger == wijs){
						data[i].pos = tmp_P_wijs_pos3[n_P_wijs_pos3] + 1;
						n_P_wijs_pos3++;
					}else{
						data[i].pos = tmp_P_pink_pos3[n_P_pink_pos3] + 1;
						n_P_pink_pos3++;					
					}			
				}			
			}else if(data[i].aantal == 4){
				data[i].pos = -1;
			}
						
			//restrictie: max 4 keer zelfde stimulus (W/P)
            
			if(i == 0){
				stimrep = 1;
			}else{
				if(data[i].blok != data[i-1].blok){
					stimrep = 1;				
				}else{			
					if(data[i].cue == data[i-1].cue)
						stimrep++;
					else
						stimrep = 1;
				}
			}
            
			if(stimrep > MAXREP){
				ok = 0;
				iteratie++;
				if(iteratie > MAXIT)
					ts5_fatal("Geen oplossing na %d iteraties", MAXIT);
				else
					break;
			}else{
				ok = 1;
			}

			//congruentie
		    
			if(data[i].cue == W){
				if(data[i].vinger == wijs)
					data[i].con = 1;
				else
					data[i].con = 0;
			}else{
				if(data[i].vinger == wijs)
					data[i].con = 0;
				else
					data[i].con = 1;			
			}
			
			//verwachte respons
			
			if(data[i].cue == W){
				data[i].xr = 1;	
			}else{
				data[i].xr = 2;			
			}		
		}	
	}while(!ok);
}


#ifndef TRIAL_C
int main()
{

	int ppn = 2;

	initialiseer();	
	randomisatie(ppn);
	randomisatie_oefen();
	schrijf();	

	return 0;
}
#endif