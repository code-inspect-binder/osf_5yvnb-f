#include <tscope5.h>
#include <stdio.h>

//////////////////////////////////////////////////////////////////
///////////				DEFINITIONS			//////////////////////
/////////////////////////////////////////////////////////////////

//within vars

#define CUE	2
#define	VINGER	2
#define	AANTAL	4

//block vars

#define	BLOK	4

//other

#define PPN	40

//design exp

#define NCELLS	(CUE * VINGER * AANTAL)
#define NREPS	6
#define NTRIALS (NCELLS * NREPS * BLOK)

#define	NTRIALS_TEST	4

//design pos: gelijk verdelen over cue x vinger x aantal elke 2 blokken

#define POS1	4
#define	POS2	6
#define POS3	4

#define BLOK_POS	2

#define NTRIALS_POS	(NTRIALS / (CUE * VINGER * AANTAL))
#define NBLOKTRIALS_POS	(NTRIALS_POS / BLOK_POS)

	//aantal 1

#define NCELLS_POS1	POS1
#define NREPS_POS1	(NBLOKTRIALS_POS / NCELLS_POS1)	

	//aantal 2
	
#define NCELLS_POS2	POS2
#define NREPS_POS2	(NBLOKTRIALS_POS / NCELLS_POS2)	

	//aantal 3
	
#define NCELLS_POS3	POS3
#define NREPS_POS3	(NBLOKTRIALS_POS / NCELLS_POS3)	

//design practice

#define NCELLS_OEFEN (CUE * VINGER * AANTAL)
#define NREPS_OEFEN	1
#define NTRIALS_OEFEN	(NCELLS_OEFEN * NREPS_OEFEN)

#define NTRIALS_TEST_OEFEN	4

//////////////////////////////////////////////////////////////////
///////////				VARS EXP			//////////////////////
/////////////////////////////////////////////////////////////////

struct{

	//descriptive

	int ppn;
	int lftd;
	char mv;

	//design
	
	int blok;
	
	enum{W = 1, P} cue;	//cue W = wijsvinger abductie, cue P = pink abductie
	enum{wijs = 1, pink} vinger;	//geobserveerde vingerbeweging: wijsvinger of pink
	int aantal;	//1 tot 4 handen die beweging maken
	
	int pos;	//1 beweging = 4 configuraties, 2 bewegingen = 6 configuraties, 3 bewegingen = 4 configuraties, 4 bewegingen = 1 configuratie	
	
	int con;	//0 = IC, 1 = C

	//response

	int xr;	
	int r;
	int corr;
	double rt;
	double re;

}data[NTRIALS];

//////////////////////////////////////////////////////////////////
///////////			VARS OEFEN				//////////////////////
/////////////////////////////////////////////////////////////////

struct{

	//design
	
	enum{W_oefen = 1, P_oefen} cue;
	enum{wijs_oefen = 1, pink_oefen} vinger;
	int aantal;
	
	int pos;
	
	int con;

	//response

	int xr;	
	int r;
	int corr;

}oefen[NTRIALS_OEFEN];

//////////////////////////////////////////////////////////////////
///////////			INITIALISATIE			//////////////////////
/////////////////////////////////////////////////////////////////

void initialiseer()
{

	int i;
	for(i = 0; i < NTRIALS; i++){

		//descriptive

		data[i].ppn = -1;
		data[i].lftd = -1;
		data[i].mv = 'x';

		//design
		
		data[i].blok = -1;
		
		data[i].cue = -1;	
		data[i].vinger = -1;	
		data[i].aantal = -1;
		
		data[i].pos = -1;	
		
		data[i].con = -1;	

		//response

		data[i].xr = -1;	
		data[i].r = -1;
		data[i].corr = -1;
		data[i].rt = -1;
		data[i].re = -1;		

	}

}

//////////////////////////////////////////////////////////////////
///////////				SCHRIJVEN			//////////////////////
/////////////////////////////////////////////////////////////////

void schrijf()
{

    FILE *fp;
    char fname[30];
    sprintf(fname, "data%02d.txt", data[0].ppn);
    fp = fopen(fname, "a+");
    if (fp == NULL)
        ts5_fatal("file %s schrijven lukt niet\n", fname);
		
	int i;
	for(i = 0; i < NTRIALS; i++){

		//descriptive

		fprintf(fp, "%02d\t", data[i].ppn);
		fprintf(fp, "%2d\t", data[i].lftd);
		fprintf(fp, "%c\t", data[i].mv);

		//design
		
		fprintf(fp, "%2d\t", data[i].blok);
		
		fprintf(fp, "%2d\t", data[i].cue);
		fprintf(fp, "%2d\t", data[i].vinger);
		fprintf(fp, "%2d\t", data[i].aantal);
		
		fprintf(fp, "%2d\t", data[i].pos);
		
		fprintf(fp, "%2d\t", data[i].con);		

		//response
		
		fprintf(fp, "%2d\t", data[i].xr);
		fprintf(fp, "%2d\t", data[i].r);
		fprintf(fp, "%2d\t", data[i].corr);
		fprintf(fp, "%6.2f\t", data[i].rt);
		fprintf(fp, "%6.2f\n", data[i].re);
	}

	fflush(fp);
	fclose(fp);	
}

#ifndef RND_C
int main()
{
	initialiseer();
	schrijf();
	return 0;
}
#endif