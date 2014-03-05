/*
 *********************************************
 *  314 Principles of Programming Languages  *
 *  Spring 2014                              *
 *  Authors: Ulrich Kremer                   *
 *           Hans Christian Woithe           *
 *********************************************
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "InstrUtils.h"
#include "Utils.h"

 /* Routines for dead code optimization */
static void markCriticals(Instruction instr);
static void	criticalStore(Instruction instr);
static void deleteNonCriticals(Instruction instr);

static void markCriticals(Instruction instr) 
{
	/* all read and write instructions are critical */
	while(instr) {
		switch (instr->opcode) {
		case READ:
			instr->critical = 1;
			printf("critical read. \n");
			break;
		case WRITE:
			instr->critical = 1;
			printf("critical write. \n");
			criticalStore(instr);
			break;
		default:
			ERROR("Illegal instructions\n");
		}
		instr = instr->next;
	}
}

static void	criticalStore(Instruction instr)
{
	/* WRITE was executed on "writeVar", need to find where "writeVar" was stored */
	char writeVar = instr->field1;

	/* begin traversing through instruction list to find critical STORE instr */
	instr = instr->prev;
	while (instr) {
		if (instr->opcode == STORE) {
			if (instr->field1 == writeVar) {
				instr->critical = 1; // found critical store instr
				printf("critical store. \n");
				/* find critical register for store! */
			}
		}
		instr = instr->prev;
	}
}

int main()
{
	Instruction *head;

	head = ReadInstructionList(stdin);
	if (!head) {
		WARNING("No instructions\n");
		exit(EXIT_FAILURE);
	}

	/* YOUR CODE GOES HERE */
	markCriticals(head);
	//deleteNonCriticals(head);

	if (head) {
		PrintInstructionList(stdout, head);
		DestroyInstructionList(head);
	}
	return EXIT_SUCCESS;
}

