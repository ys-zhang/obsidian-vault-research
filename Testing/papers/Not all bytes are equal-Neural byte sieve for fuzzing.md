---

excalidraw-plugin: raw

---
==⚠  Switch to EXCALIDRAW VIEW in the MORE OPTIONS menu of this document. ⚠==


# Text Elements
We present a learning
technique that uses neural networks to learn patterns in the
input files from past fuzzing explorations to guide future fuzzing
explorations. ^egJ0JKGO

use machine learning to learn a strategy for guiding the input mutations
based on previous history of executed inputs and code coverage
information. ^D9pLhKhV

Fuzzer ^WbBgcxIt

seed ^Gm840jKp

ML Model ^bRt4gacR

evaluate/choosing mutation that
maximize coverage gain ^L8PjESA2

How to choose mutations (where to mutate? mutate to what?)
    that MAXIMISE coverage gain? ^MWO76IFi

Fuzzer ^D85D88qC

seed ^1Jc646do

ML Model ^2xVRjfD9

mutation ^qY5HPizI

Target Program ^ETPGUyTA

Mutates good positions ^zK9Rqc46

mutates bad positions ^vmuwEcNW

How to assess input positions, where shall we mutate? ^HGDXzYvu

f(mutation|x) = Gain(coverage(x), coverage(mutate(x))) ^V6GvCBBd

this is what we want to learn ^WYF6gXul

How to define the gain of coverage? ^K2W19Hhs

Gain(b, b') = sum of new ones in the coverage bitmap ^j9FL9ewg

The goal of
augmented fuzzing is to improve the hit-rate of mutations.
Using the annotated seed provided by the model, mutations
that are unlikely to give input gain are avoided. 

We used a
highly permissive veto approach to reject mutations which
target no useful locations. ^bIORBHm7

why this works ^k0oo2n1V

%%
# Drawing
```json
{
	"type": "excalidraw",
	"version": 2,
	"source": "https://excalidraw.com",
	"elements": [
		{
			"type": "text",
			"version": 72,
			"versionNonce": 2140706217,
			"isDeleted": false,
			"id": "egJ0JKGO",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -347.33331298828125,
			"y": -282.4999694824219,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 558,
			"height": 93,
			"seed": 567108777,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 2,
			"text": "We present a learning\ntechnique that uses neural networks to learn patterns in the\ninput files from past fuzzing explorations to guide future fuzzing\nexplorations.",
			"rawText": "We present a learning\ntechnique that uses neural networks to learn patterns in the\ninput files from past fuzzing explorations to guide future fuzzing\nexplorations.",
			"baseline": 89,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 43,
			"versionNonce": 1169301385,
			"isDeleted": false,
			"id": "D9pLhKhV",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -344.6666259765625,
			"y": -139.83331298828125,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 629,
			"height": 70,
			"seed": 1416165991,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 2,
			"text": "use machine learning to learn a strategy for guiding the input mutations\nbased on previous history of executed inputs and code coverage\ninformation.",
			"rawText": "use machine learning to learn a strategy for guiding the input mutations\nbased on previous history of executed inputs and code coverage\ninformation.",
			"baseline": 65,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "rectangle",
			"version": 103,
			"versionNonce": 258595495,
			"isDeleted": false,
			"id": "9b_qeI8SoQoixA_wtK0eI",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -380,
			"y": 80,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 120,
			"height": 80,
			"seed": 1804955815,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"j_cUoyyGkmq9fN2rXGgPU"
			]
		},
		{
			"type": "text",
			"version": 80,
			"versionNonce": 649407785,
			"isDeleted": false,
			"id": "WbBgcxIt",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -360,
			"y": 100,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 65,
			"height": 25,
			"seed": 1588196041,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "Fuzzer",
			"rawText": "Fuzzer",
			"baseline": 18,
			"textAlign": "center",
			"verticalAlign": "middle"
		},
		{
			"type": "arrow",
			"version": 354,
			"versionNonce": 1291741833,
			"isDeleted": false,
			"id": "j_cUoyyGkmq9fN2rXGgPU",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -252.66066450467883,
			"y": 116.99953600280891,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 126.4495345212398,
			"height": 1.2449263438902989,
			"seed": 1767924969,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "9b_qeI8SoQoixA_wtK0eI",
				"gap": 7.339335495321152,
				"focus": -0.05758494535414237
			},
			"endBinding": {
				"elementId": "jX6HNUazpm-hRHdi7PiTS",
				"gap": 6.405731041482854,
				"focus": 0.11750305858618763
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					126.4495345212398,
					-1.2449263438902989
				]
			]
		},
		{
			"type": "ellipse",
			"version": 24,
			"versionNonce": 12760487,
			"isDeleted": false,
			"id": "jX6HNUazpm-hRHdi7PiTS",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -120,
			"y": 80,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 80,
			"height": 80,
			"seed": 976624231,
			"groupIds": [
				"g1_UmHBlbWinEl4aI8IY-"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"j_cUoyyGkmq9fN2rXGgPU",
				"w7zh155_i3UxnkjvGnvLz"
			]
		},
		{
			"type": "text",
			"version": 19,
			"versionNonce": 577560519,
			"isDeleted": false,
			"id": "Gm840jKp",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -102,
			"y": 107.5,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 44,
			"height": 25,
			"seed": 1094571785,
			"groupIds": [
				"g1_UmHBlbWinEl4aI8IY-"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "seed",
			"rawText": "seed",
			"baseline": 18,
			"textAlign": "center",
			"verticalAlign": "middle"
		},
		{
			"type": "arrow",
			"version": 76,
			"versionNonce": 1781144649,
			"isDeleted": false,
			"id": "w7zh155_i3UxnkjvGnvLz",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -40,
			"y": 120,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 120,
			"height": 0,
			"seed": 1261446761,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "jX6HNUazpm-hRHdi7PiTS",
				"focus": 0,
				"gap": 1
			},
			"endBinding": {
				"elementId": "Xuv73aIpGzW8NemekRXd9",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					120,
					0
				]
			]
		},
		{
			"type": "diamond",
			"version": 26,
			"versionNonce": 551254121,
			"isDeleted": false,
			"id": "Xuv73aIpGzW8NemekRXd9",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 80,
			"y": 80,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 160,
			"height": 80,
			"seed": 88087049,
			"groupIds": [
				"oyVpz4Im7Be3kqFkmG3wi"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"w7zh155_i3UxnkjvGnvLz",
				"b75O3MZrlxCVM7BWNurnD"
			]
		},
		{
			"type": "text",
			"version": 28,
			"versionNonce": 1155708681,
			"isDeleted": false,
			"id": "bRt4gacR",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 114.5,
			"y": 107.5,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 91,
			"height": 25,
			"seed": 1541488457,
			"groupIds": [
				"oyVpz4Im7Be3kqFkmG3wi"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "ML Model",
			"rawText": "ML Model",
			"baseline": 18,
			"textAlign": "center",
			"verticalAlign": "middle"
		},
		{
			"type": "arrow",
			"version": 12,
			"versionNonce": 1333631433,
			"isDeleted": false,
			"id": "b75O3MZrlxCVM7BWNurnD",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 160,
			"y": -40,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 20,
			"height": 120,
			"seed": 579909607,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "L8PjESA2",
				"focus": 0.9799498746867168,
				"gap": 1
			},
			"endBinding": {
				"elementId": "Xuv73aIpGzW8NemekRXd9",
				"focus": -0.33333333333333337,
				"gap": 8.94427190999916
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-20,
					120
				]
			]
		},
		{
			"type": "text",
			"version": 87,
			"versionNonce": 605586247,
			"isDeleted": false,
			"id": "L8PjESA2",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 160,
			"y": -60,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 324,
			"height": 50,
			"seed": 340849095,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"b75O3MZrlxCVM7BWNurnD"
			],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "evaluate/choosing mutation that\nmaximize coverage gain",
			"rawText": "evaluate/choosing mutation that\nmaximize coverage gain",
			"baseline": 43,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 123,
			"versionNonce": 2079218281,
			"isDeleted": false,
			"id": "MWO76IFi",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -380,
			"y": 200,
			"strokeColor": "#862e9c",
			"backgroundColor": "transparent",
			"width": 876,
			"height": 70,
			"seed": 2016420457,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "How to choose mutations (where to mutate? mutate to what?)\n    that MAXIMISE coverage gain?",
			"rawText": "How to choose mutations (where to mutate? mutate to what?)\n    that MAXIMISE coverage gain?",
			"baseline": 60,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 106,
			"versionNonce": 1279158183,
			"isDeleted": false,
			"id": "D85D88qC",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -380,
			"y": 420,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 65,
			"height": 25,
			"seed": 431261031,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "Fuzzer",
			"rawText": "Fuzzer",
			"baseline": 18,
			"textAlign": "center",
			"verticalAlign": "middle"
		},
		{
			"type": "arrow",
			"version": 510,
			"versionNonce": 1651731561,
			"isDeleted": false,
			"id": "7bm8bVvNKn5JvoFZqD-Yx",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -272.66066450467883,
			"y": 436.9995360028089,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 126.4495345212398,
			"height": 1.2449263438902989,
			"seed": 1842227337,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "Sme3Evo6juuST2-97hO1l",
				"focus": -0.05758686262011673,
				"gap": 7.339335495321166
			},
			"endBinding": {
				"elementId": "XfzGTTAEcEB-fmGHNpUBI",
				"focus": 0.11750305858618851,
				"gap": 6.405731041482866
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					126.4495345212398,
					-1.2449263438902989
				]
			]
		},
		{
			"type": "ellipse",
			"version": 65,
			"versionNonce": 1834242983,
			"isDeleted": false,
			"id": "XfzGTTAEcEB-fmGHNpUBI",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -140,
			"y": 400,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 80,
			"height": 80,
			"seed": 1005404295,
			"groupIds": [
				"UFLWVbvjeFxuk_Ys1vjXj"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"7bm8bVvNKn5JvoFZqD-Yx",
				"5hBAnJFDYrPk8jmpNgLf6",
				"CtVs8edZv6lVajjuLkDJ0",
				"OCZDH5nr4gcsj_BXOykWT"
			]
		},
		{
			"type": "text",
			"version": 45,
			"versionNonce": 568124137,
			"isDeleted": false,
			"id": "1Jc646do",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -122,
			"y": 427.5,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 44,
			"height": 25,
			"seed": 887762793,
			"groupIds": [
				"UFLWVbvjeFxuk_Ys1vjXj"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "seed",
			"rawText": "seed",
			"baseline": 18,
			"textAlign": "center",
			"verticalAlign": "middle"
		},
		{
			"type": "arrow",
			"version": 281,
			"versionNonce": 488883977,
			"isDeleted": false,
			"id": "5hBAnJFDYrPk8jmpNgLf6",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -59,
			"y": 440,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 118,
			"height": 0,
			"seed": 161585063,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "XfzGTTAEcEB-fmGHNpUBI",
				"focus": 0,
				"gap": 1
			},
			"endBinding": {
				"elementId": "azgaKWRDY1IdjAb2eXpWM",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					118,
					0
				]
			]
		},
		{
			"type": "diamond",
			"version": 65,
			"versionNonce": 1482226281,
			"isDeleted": false,
			"id": "azgaKWRDY1IdjAb2eXpWM",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 60,
			"y": 400,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 160,
			"height": 80,
			"seed": 211966537,
			"groupIds": [
				"gxKJOKTEKjLCKTertl4FN"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"5hBAnJFDYrPk8jmpNgLf6",
				"ADeriNe-F1ONNsrtVWZUH",
				"OCZDH5nr4gcsj_BXOykWT",
				"IsMfRTnrbECgHH6nZQa6M"
			]
		},
		{
			"type": "text",
			"version": 54,
			"versionNonce": 1123408263,
			"isDeleted": false,
			"id": "2xVRjfD9",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 94.5,
			"y": 427.5,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 91,
			"height": 25,
			"seed": 1224608455,
			"groupIds": [
				"gxKJOKTEKjLCKTertl4FN"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "ML Model",
			"rawText": "ML Model",
			"baseline": 18,
			"textAlign": "center",
			"verticalAlign": "middle"
		},
		{
			"type": "rectangle",
			"version": 125,
			"versionNonce": 1246278535,
			"isDeleted": false,
			"id": "Sme3Evo6juuST2-97hO1l",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -400,
			"y": 400,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 120,
			"height": 80,
			"seed": 566734825,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"j_cUoyyGkmq9fN2rXGgPU",
				"7bm8bVvNKn5JvoFZqD-Yx"
			]
		},
		{
			"type": "arrow",
			"version": 125,
			"versionNonce": 760836073,
			"isDeleted": false,
			"id": "CtVs8edZv6lVajjuLkDJ0",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -102.84641676604951,
			"y": 480.90107469974294,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 2.846416766049515,
			"height": 119.09892530025706,
			"seed": 964375337,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "XfzGTTAEcEB-fmGHNpUBI",
				"focus": 0.09557111227527659,
				"gap": 1
			},
			"endBinding": {
				"elementId": "UQjzUBZDl2R8pw8P25wcu",
				"focus": -0.1808472669483489,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					2.846416766049515,
					119.09892530025706
				]
			]
		},
		{
			"type": "ellipse",
			"version": 18,
			"versionNonce": 1001660999,
			"isDeleted": false,
			"id": "UQjzUBZDl2R8pw8P25wcu",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -140,
			"y": 600,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 100,
			"height": 80,
			"seed": 950592553,
			"groupIds": [
				"8BLWf2G6WItGoMcS3yJ4O"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"CtVs8edZv6lVajjuLkDJ0",
				"ndM9E4X7g7-Y992ve3co2"
			]
		},
		{
			"type": "text",
			"version": 11,
			"versionNonce": 1296954121,
			"isDeleted": false,
			"id": "qY5HPizI",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -132.5,
			"y": 627.5,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 85,
			"height": 25,
			"seed": 258215593,
			"groupIds": [
				"8BLWf2G6WItGoMcS3yJ4O"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "mutation",
			"rawText": "mutation",
			"baseline": 18,
			"textAlign": "center",
			"verticalAlign": "middle"
		},
		{
			"type": "arrow",
			"version": 51,
			"versionNonce": 583748009,
			"isDeleted": false,
			"id": "ndM9E4X7g7-Y992ve3co2",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -40,
			"y": 600,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 140,
			"height": 120,
			"seed": 1700316425,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "UQjzUBZDl2R8pw8P25wcu",
				"focus": 0.04873701788285782,
				"gap": 18.429078977666123
			},
			"endBinding": null,
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					140,
					-120
				]
			]
		},
		{
			"type": "arrow",
			"version": 132,
			"versionNonce": 220645895,
			"isDeleted": false,
			"id": "OCZDH5nr4gcsj_BXOykWT",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 127.04167638452715,
			"y": 396.47916180773643,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 201.87495885523026,
			"height": 69.81261212511924,
			"seed": 132441383,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "azgaKWRDY1IdjAb2eXpWM",
				"focus": 0.5903157803210995,
				"gap": 8.94427190999916
			},
			"endBinding": {
				"elementId": "XfzGTTAEcEB-fmGHNpUBI",
				"focus": -0.6300776570342392,
				"gap": 12.434441832141289
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-96.54164586694903,
					-69.81261212511924
				],
				[
					-201.87495885523026,
					-2.4792381016817444
				]
			]
		},
		{
			"type": "arrow",
			"version": 177,
			"versionNonce": 1426587113,
			"isDeleted": false,
			"id": "IsMfRTnrbECgHH6nZQa6M",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 220.46605793766682,
			"y": 446.53059191131354,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 282.93408151006884,
			"height": 5.53249875621259,
			"seed": 1233056775,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "azgaKWRDY1IdjAb2eXpWM",
				"focus": 0.20260067091992187,
				"gap": 6.049566424818231
			},
			"endBinding": null,
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					282.93408151006884,
					-5.53249875621259
				]
			]
		},
		{
			"type": "text",
			"version": 31,
			"versionNonce": 234865993,
			"isDeleted": false,
			"id": "ETPGUyTA",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 520,
			"y": 420,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 158,
			"height": 25,
			"seed": 779805095,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "Target Program",
			"rawText": "Target Program",
			"baseline": 18,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 57,
			"versionNonce": 1014460873,
			"isDeleted": false,
			"id": "zK9Rqc46",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 260,
			"y": 460,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 185,
			"height": 20,
			"seed": 469730727,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 16,
			"fontFamily": 1,
			"text": "Mutates good positions",
			"rawText": "Mutates good positions",
			"baseline": 14,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 26,
			"versionNonce": 1668036329,
			"isDeleted": false,
			"id": "vmuwEcNW",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 80,
			"y": 320,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 176,
			"height": 20,
			"seed": 661792617,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 16,
			"fontFamily": 1,
			"text": "mutates bad positions",
			"rawText": "mutates bad positions",
			"baseline": 14,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 91,
			"versionNonce": 1991885959,
			"isDeleted": false,
			"id": "HGDXzYvu",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -360,
			"y": 720,
			"strokeColor": "#862e9c",
			"backgroundColor": "transparent",
			"width": 755,
			"height": 35,
			"seed": 734204775,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "How to assess input positions, where shall we mutate?",
			"rawText": "How to assess input positions, where shall we mutate?",
			"baseline": 25,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 96,
			"versionNonce": 1456570153,
			"isDeleted": false,
			"id": "V6GvCBBd",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -340,
			"y": 800,
			"strokeColor": "#087f5b",
			"backgroundColor": "transparent",
			"width": 632,
			"height": 23,
			"seed": 1434029767,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"Qu_NgoxeX66ajubSSCGZJ"
			],
			"fontSize": 20,
			"fontFamily": 3,
			"text": "f(mutation|x) = Gain(coverage(x), coverage(mutate(x)))",
			"rawText": "f(mutation|x) = Gain(coverage(x), coverage(mutate(x)))",
			"baseline": 19,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "arrow",
			"version": 51,
			"versionNonce": 1108583017,
			"isDeleted": false,
			"id": "Qu_NgoxeX66ajubSSCGZJ",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -110.16355440082057,
			"y": 959,
			"strokeColor": "#087f5b",
			"backgroundColor": "transparent",
			"width": 8.771849829179388,
			"height": 118.99999999999989,
			"seed": 415220935,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "WYF6gXul",
				"focus": -0.9159663865546218,
				"gap": 1
			},
			"endBinding": {
				"elementId": "V6GvCBBd",
				"focus": 0.5033860045146726,
				"gap": 6
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-8.771849829179388,
					-118.99999999999989
				]
			]
		},
		{
			"type": "text",
			"version": 52,
			"versionNonce": 654653415,
			"isDeleted": false,
			"id": "WYF6gXul",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -120,
			"y": 960,
			"strokeColor": "#862e9c",
			"backgroundColor": "transparent",
			"width": 297,
			"height": 25,
			"seed": 1703926889,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"Qu_NgoxeX66ajubSSCGZJ"
			],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "this is what we want to learn",
			"rawText": "this is what we want to learn",
			"baseline": 18,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 54,
			"versionNonce": 515902535,
			"isDeleted": false,
			"id": "K2W19Hhs",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -360,
			"y": 1040,
			"strokeColor": "#862e9c",
			"backgroundColor": "transparent",
			"width": 501,
			"height": 35,
			"seed": 868563241,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "How to define the gain of coverage?",
			"rawText": "How to define the gain of coverage?",
			"baseline": 25,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 73,
			"versionNonce": 905460039,
			"isDeleted": false,
			"id": "j9FL9ewg",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -340,
			"y": 1100,
			"strokeColor": "#087f5b",
			"backgroundColor": "transparent",
			"width": 609,
			"height": 23,
			"seed": 33497737,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 3,
			"text": "Gain(b, b') = sum of new ones in the coverage bitmap",
			"rawText": "Gain(b, b') = sum of new ones in the coverage bitmap",
			"baseline": 19,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 16,
			"versionNonce": 1269140519,
			"isDeleted": false,
			"id": "bIORBHm7",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -380,
			"y": 1220,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 680,
			"height": 187,
			"seed": 431084423,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"SYVAQpz7NfjL8vmOJffFu"
			],
			"fontSize": 20,
			"fontFamily": 3,
			"text": "The goal of\naugmented fuzzing is to improve the hit-rate of mutations.\nUsing the annotated seed provided by the model, mutations\nthat are unlikely to give input gain are avoided. \n\nWe used a\nhighly permissive veto approach to reject mutations which\ntarget no useful locations.",
			"rawText": "The goal of\naugmented fuzzing is to improve the hit-rate of mutations.\nUsing the annotated seed provided by the model, mutations\nthat are unlikely to give input gain are avoided. \n\nWe used a\nhighly permissive veto approach to reject mutations which\ntarget no useful locations.",
			"baseline": 183,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "arrow",
			"version": 160,
			"versionNonce": 1193658599,
			"isDeleted": false,
			"id": "SYVAQpz7NfjL8vmOJffFu",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 459,
			"y": 1337.3348642263772,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 157.99999999999994,
			"height": 26.80271830461993,
			"seed": 1908752967,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "k0oo2n1V",
				"focus": 0.29577464788732394,
				"gap": 1
			},
			"endBinding": {
				"elementId": "bIORBHm7",
				"focus": 0.6896551724137931,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-157.99999999999994,
					26.80271830461993
				]
			]
		},
		{
			"type": "text",
			"version": 30,
			"versionNonce": 1284575431,
			"isDeleted": false,
			"id": "k0oo2n1V",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 460,
			"y": 1320,
			"strokeColor": "#862e9c",
			"backgroundColor": "transparent",
			"width": 140,
			"height": 25,
			"seed": 1269211399,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"SYVAQpz7NfjL8vmOJffFu"
			],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "why this works",
			"rawText": "why this works",
			"baseline": 18,
			"textAlign": "left",
			"verticalAlign": "top"
		}
	],
	"appState": {
		"theme": "light",
		"viewBackgroundColor": "#ffffff",
		"currentItemStrokeColor": "#862e9c",
		"currentItemBackgroundColor": "transparent",
		"currentItemFillStyle": "hachure",
		"currentItemStrokeWidth": 1,
		"currentItemStrokeStyle": "solid",
		"currentItemRoughness": 1,
		"currentItemOpacity": 100,
		"currentItemFontFamily": 1,
		"currentItemFontSize": 20,
		"currentItemTextAlign": "left",
		"currentItemStrokeSharpness": "sharp",
		"currentItemStartArrowhead": null,
		"currentItemEndArrowhead": "arrow",
		"currentItemLinearStrokeSharpness": "round",
		"gridSize": 20
	},
	"files": {}
}
```
%%