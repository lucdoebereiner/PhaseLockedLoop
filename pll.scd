
(
Server.default.options.blockSize = 1;
Server.default.options.numInputBusChannels = 3;
Server.default.options.numOutputBusChannels = 6;

Server.default.reboot;
)


(
~pll1 = Bus.audio(s, 1);
~pll2 = Bus.audio(s, 1);
~pll3 = Bus.audio(s, 1);
~pll4 = Bus.audio(s, 1);
~pll5 = Bus.audio(s, 1);
~pll6 = Bus.audio(s, 1);
)


{ Out.ar(0, PinkNoise.ar(1!6) * 1) }.play

{ Out.ar(3, SinOsc.ar(100!3) * 1) }.play


s.meter

OSCFunc.trace(true, true)



(
~addr1 = NetAddr.new("192.168.0.50", 6666);
~addr2 = NetAddr.new("192.168.0.102", 6666);
~addr3 = NetAddr.new("192.168.0.103", 6666);

OSCdef(\ready1, { arg msg; "READY 1: ".post; msg[1].postln; }, \ready, ~addr1);
OSCdef(\ready2, { arg msg; "READY 2: ".post; msg[1].postln; }, \ready, ~addr2);
OSCdef(\ready3, { arg msg; "READY 3: ".post; msg[1].postln; }, \ready, ~addr3);

~pieceFunc = {
	var amp1, amp2, amp3;



	~controls1 = LBLControlCollection(~addr1);
	~controls2 = LBLControlCollection(~addr2);
	~controls3 = LBLControlCollection(~addr3);

	// create controls
	~controls1.add(LBLControl(\freqCont, ControlSpec(0.04, 3, 5, units: "GFK"), 0.2, \pll1, \freq1, '/freqLabel'));
	~controls1.add(LBLControlXY(\pllXY, [ControlSpec(0.5, 0.93, -2, units: "Leak"), ControlSpec(0.001, 10, 2, units: "Freq")], [0.5, 0.08], \pll1,
		[\leak1, \errFreq1], ['/pllLeak', '/pllFreq']));
	~controls1.add(LBLControl(\amp, ControlSpec(0.0, 1, 2, units: "Amp"), 0.5, \pll1, \amp1, '/ampLabel'));
	~controls1.add(LBLControl(\loopMax, ControlSpec(0.0, 1, 1, units: "FreqMax"), 0.0, \fbpll1, \freqMax, '/freqMax', { arg v, s; s.set(\loop, v[1].asInteger)}));
	~controls1.add(LBLControl(\extIn, ControlSpec(0.0, 0.98, 1, units: "ExtIn"), 0.0, \fbpll1, \extAmount, '/extAmount'));
	~controls1.add(LBLControlXYMLP(\xy2, \fbpll1, {arg val, synth; synth.set(\amp, val)}, "/home/luc/Dropbox/music/lbl/mpl/model1/paramsdata.json"));

	~controls2.add(LBLControl(\freqCont, ControlSpec(0.04, 3, 5, units: "GFK"), 0.2, \pll1, \freq2, '/freqLabel'));
	~controls2.add(LBLControlXY(\pllXY, [ControlSpec(0.5, 0.93, -2, units: "Leak"), ControlSpec(0.001, 10, 2, units: "Freq")], [0.5, 0.08], \pll1,
		[\leak2, \errFreq2], ['/pllLeak', '/pllFreq']));
	~controls2.add(LBLControl(\amp, ControlSpec(0.0, 1, 2, units: "Amp"), 0.5, \pll1, \amp2, '/ampLabel'));
	~controls2.add(LBLControl(\loopMax, ControlSpec(0.0, 1, 1, units: "FreqMax"), 0.0, \fbpll2, \freqMax, '/freqMax', { arg v, s; s.set(\loop, v[1].asInteger)}));
	~controls2.add(LBLControl(\extIn, ControlSpec(0.0, 0.98, 1, units: "ExtIn"), 0.0, \fbpll2, \extAmount, '/extAmount'));
	~controls2.add(LBLControlXYMLP(\xy2, \fbpll2, {arg val, synth; synth.set(\amp, val)}, "/home/luc/Dropbox/music/lbl/mpl/model1/paramsdata.json"));


	~controls3.add(LBLControl(\freqCont, ControlSpec(0.04, 3, 5, units: "GFK"), 0.2, \pll1, \freq3, '/freqLabel'));
	~controls3.add(LBLControlXY(\pllXY, [ControlSpec(0.5, 0.93, -2, units: "Leak"), ControlSpec(0.001, 10, 2, units: "Freq")], [0.5, 0.08], \pll1,
		[\leak3, \errFreq3], ['/pllLeak', '/pllFreq']));
	~controls3.add(LBLControl(\amp, ControlSpec(0.0, 1, 2, units: "Amp"), 0.5, \pll1, \amp3, '/ampLabel'));
	~controls3.add(LBLControl(\loopMax, ControlSpec(0.0, 1, 1, units: "FreqMax"), 0.0, \fbpll3, \freqMax, '/freqMax', { arg v, s; s.set(\loop, v[1].asInteger)}));
	~controls3.add(LBLControl(\extIn, ControlSpec(0.0, 0.98, 1, units: "ExtIn"), 0.0, \fbpll3, \extAmount, '/extAmount'));
	~controls3.add(LBLControlXYMLP(\xy2, \fbpll3, {arg val, synth; synth.set(\amp, val)}, "/home/luc/Dropbox/music/lbl/mpl/model1/paramsdata.json"));



	// set initial parameters
	~controls1.setParameterList([\freq1, 0.08, \hpFreq1, 1200, \combAmp1, 0.0, \lpfHpf1, 0.2, \errFreq1, 0, \leak1, 0.0, \rq1, 0.1, \fil1, 300, \amp1, 0.5]);
	~controls2.setParameterList([\freq2, 0.08, \hpFreq2, 1200, \combAmp2, 0.0, \lpfHpf2, 0.2, \errFreq2, 0, \leak2, 0.0, \rq2, 0.1, \fil2, 300, \amp2, 0.5]);
	~controls3.setParameterList([\freq3, 0.08, \hpFreq3, 1200, \combAmp3, 0.0, \lpfHpf3, 0.2, \errFreq3, 0, \leak3, 0.0, \rq3, 0.1, \fil3, 300, \amp3, 0.5]);

	~addr1.sendMsg(\time, "-5");
	~addr2.sendMsg(\time, "-5");
	~addr3.sendMsg(\time, "-5");


	OSCdef(\mute1, {arg msg;
		amp1 = ~controls1.getVal(\amp);
		if (msg[1] > 0) {
			~synthDict.at(\pll1).set(\ampLag1, 0.02);
			~synthDict.at(\pll1).set(\amp1, 0.0);
			{ ~synthDict.at(\pll1).set(\filModSpeed1, 10) }.defer(0.05);

		} {
			~synthDict.at(\pll1).set(\filModSpeed1, 0.06);
			~synthDict.at(\pll1).set(\amp1, amp1);
			{ ~synthDict.at(\pll1).set(\ampLag1, 0.2) }.defer(0.5);
		};
	}, \mute, ~addr1);

	OSCdef(\mute2, {arg msg;
		amp2 = ~controls2.getVal(\amp);
		if (msg[1] > 0) {
			~synthDict.at(\pll1).set(\ampLag2, 0.02);
			~synthDict.at(\pll1).set(\amp2, 0.0);
			{ ~synthDict.at(\pll1).set(\filModSpeed2, 10) }.defer(0.05);

		} {
			~synthDict.at(\pll1).set(\filModSpeed2, 0.06);
			~synthDict.at(\pll1).set(\amp2, amp2);
			{ ~synthDict.at(\pll1).set(\ampLag2, 0.2) }.defer(0.5);
		};
	}, \mute, ~addr2);

	OSCdef(\mute3, {arg msg;
		amp3 = ~controls3.getVal(\amp);
		if (msg[1] > 0) {
			~synthDict.at(\pll1).set(\ampLag3, 0.02);
			~synthDict.at(\pll1).set(\amp3, 0.0);
			{ ~synthDict.at(\pll1).set(\filModSpeed3, 10) }.defer(0.05);
		} {
			~synthDict.at(\pll1).set(\filModSpeed3, 0.06);
			~synthDict.at(\pll1).set(\amp3, amp3);
			{ ~synthDict.at(\pll1).set(\ampLag3, 0.2) }.defer(0.5);
		};
	}, \mute, ~addr3);

	~synthDict = Dictionary();
};



~addr1.sendMsg(\score, "Press READY");
~addr2.sendMsg(\score, "Press READY");
~addr3.sendMsg(\score, "Press READY");

~addr1.sendMsg(\pll2Label, "");
~addr2.sendMsg(\pll2Label, "");
~addr3.sendMsg(\pll2Label, "");


~piece =  LBL(
	[
		[0, {
			[~addr1, ~addr2, ~addr3].do({arg a; a.sendMsg(\score, "keine Aktion")});

			"Section 1".postln;
			~synthDict.put(\pll1, Synth(\pll, [\b1, ~pll1, \b2, ~pll2, \b3, ~pll3, \freq1, 0.08, \freq2, 0.08, \freq3, 0.08, \hpFreq1, 1200, \hpFreq2, 1400, \hpFreq3, 800, \hpf1amp, 0.7, \hpf2amp, 0.7, \hpf3amp, 0.7, \filSel, 1, \lpfHpfMode, 1, \combHighAmp, 0.8, \combOnOff1, 1, \combOffWeight, 4, \combAmp1, 0.0, \combAmp2, 0.0, \combAmp3, 0.0,\combOnOff1, 1, \combOnOff1, 2, \combOnOff3, 1,\combOffWeight, 4, \lpfHpf1, 0.2, \amp1, 0.5, \amp2, 0.5, \amp3, 0.5, \combRate1, 0.05, \combRate2, 0.05, \combRate3, 0.05, \amp, 0.7]));

			~synthDict.put(\fbpll1, Synth(\fbpll, [\out1, 0, \out2, 3, \paramsbuf, ~controls1.get(\xy2).paramsbuf, \recBuf, ~recBuf1.bufnum, \pllBus1, ~pll4, \pllBus2, ~pll5, \extIn, ~pll6, \extIn2, ~pll1, \amp2, 0.95]));
			~synthDict.put(\fbpll2, Synth(\fbpll, [\out1, 1, \out2, 4, \paramsbuf, ~controls2.get(\xy2).paramsbuf, \recBuf, ~recBuf2.bufnum, \pllBus1, ~pll6, \pllBus2, ~pll7, \extIn, ~pll8, \extIn2, ~pll2, \amp2, 0.95]));
			~synthDict.put(\fbpll3, Synth(\fbpll, [\out1, 2, \out2, 5, \paramsbuf, ~controls3.get(\xy2).paramsbuf, \recBuf, ~recBuf3.bufnum, \pllBus1, ~pll8, \pllBus2, ~pll9, \extIn, ~pll10, \extIn2, ~pll3, \amp2, 0.95]));


			s.sync;

			~controls1.setSynthUpdate(~synthDict, true);
			~controls2.setSynthUpdate(~synthDict, true);
			~controls3.setSynthUpdate(~synthDict, true);

			~controls1.setParameterList([\freq1, 0.08, \hpFreq1, 1200, \combAmp1, 0.0, \lpfHpf1, 0.2, \errFreq1, 0, \leak1, 0.0, \rq1, 0.1, \fil1, 300, \amp1, 0.5]);
			~controls2.setParameterList([\freq2, 0.08, \hpFreq2, 1200, \combAmp2, 0.0, \lpfHpf2, 0.2, \errFreq2, 0, \leak2, 0.0, \rq2, 0.1, \fil2, 300, \amp2, 0.5]);
			~controls3.setParameterList([\freq3, 0.08, \hpFreq3, 1200, \combAmp3, 0.0, \lpfHpf3, 0.2, \errFreq3, 0, \leak3, 0.0, \rq3, 0.1, \fil3, 300, \amp3, 0.5]);

			~controls1.listen();
			~controls2.listen();
			~controls3.listen();
		}],
		[50, {
			[~addr1, ~addr2, ~addr3].do({arg a; a.sendMsg(\score, "60 - 90\nKOPPLUNG: intermittierend langsame, kontinuierliche Bewegungen im unteren linken Quadranten. Auf die Desynchronisation hören.")});
		}],
		[80, {
			[~addr1, ~addr2, ~addr3].do({arg a; a.sendMsg(\score, "90 - 130\nGFK: Sprunghaft oder wellenförmig ändern. Nur gelegentlich dicht werden lassen. Immer wieder auf ~0.1 Hz zurückkehren\n\nKOPPLUNG: wie vorher")});
		}],
		[120, {
			~synthDict.at(\pll1).set(\combAmp1, 0.2);
			~synthDict.at(\pll1).set(\combOffWeight, 6);
			[~addr1, ~addr2, ~addr3].do({arg a; a.sendMsg(\score, "130 - 200\nGFK: häufiger dichte Momente \n\nKOPPLUNG: allmählich oberen rechten Qudranten miteinbeziehen ")});

		}],
		[130, {
			~synthDict.at(\pll1).set(\combAmp2, 0.2);
		}],
		[140, {
			~synthDict.at(\pll1).set(\combAmp3, 0.2);
		}],
		[190, {
			[~addr1, ~addr2, ~addr3].do({arg a; a.sendMsg(\score, "200 - 250\nGFK: Sprünge im gesamten Raum \n\nKOPPLUNG: Intermittierend hohe Rausch-Texturen durch oberen rechten Quadranten ")});
		}],
		[200, {

			~synthDict.at(\pll1).set(\combAmp1, 0.2, \combAmp2, 0.2, \combAmp3, 0.2,  \combOnOff1, 0, \combOnOff2, 0, \combOnOff3, 0);

			~controls1.setSpec(\pllXY, [ControlSpec(0.5, 0.98, 1, units: "Leak"), ControlSpec(2, 1730, 7, units: "Freq")]);
			~controls2.setSpec(\pllXY, [ControlSpec(0.5, 0.98, 1, units: "Leak"), ControlSpec(2, 1730, 7, units: "Freq")]);
			~controls3.setSpec(\pllXY, [ControlSpec(0.5, 0.98, 1, units: "Leak"), ControlSpec(2, 1730, 7, units: "Freq")]);

			~synthDict.at(\pll1).set(\filSel, 0, \lpfHpfMode, 0, \combHighAmp, 0.5, \filModSpeed, 0.065);

			~synthDict.at(\pll1).set(\combAmp1, 0.0, \combAmp2, 0.0, \combAmp3, 0.0);
			~controls1.setSpec(\freqCont, ControlSpec(0.1, 30, 3, units: "GFK"));
			~controls2.setSpec(\freqCont, ControlSpec(0.1, 30, 3, units: "GFK"));
			~controls3.setSpec(\freqCont, ControlSpec(0.1, 30, 3, units: "GFK"));
		}],
		[240, {
			[~addr1, ~addr2, ~addr3].do({arg a; a.sendMsg(\score, "250 - 400\n↓ Einsatz mit KOPPLUNG + PLL2, dann Gesten")});
		}],
		[248, {
			"Section 2".postln;

			~synthDict.at(\pll1).set(\ampLag1, 0.02, \amp1, 0, \ampLag2, 0.02, \amp2, 0, \ampLag3, 0.02, \amp3, 0);
			~controls1.get(\pllXY).func = {arg val, synth; synth.set(\amp1, val.asInteger/2)};
			~controls2.get(\pllXY).func = {arg val, synth; synth.set(\amp2, val.asInteger/2)};
			~controls3.get(\pllXY).func = {arg val, synth; synth.set(\amp3, val.asInteger/2)};
			~addr1.sendMsg(\pll2Label, "PLL2");
			~addr2.sendMsg(\pll2Label, "PLL2");
			~addr3.sendMsg(\pll2Label, "PLL2");

		}],


		[380, {
			~synthDict.at(\fbpll1).set(\extAmount, 0.92);
			~synthDict.at(\fbpll2).set(\extAmount, 0.92);
			~synthDict.at(\fbpll3).set(\extAmount, 0.92);
		}],
		[390, {
			[~addr1, ~addr2, ~addr3].do({arg a; a.sendMsg(\score, "400 - 530\nGFK: gelegentlich langsame Wellen\nKOPPLUNG: intermittierend langsame, kontinuierliche Bewegungen im ganzen Bereich\nFILTER: fast immer sehr langsame, kontinuierliche Bewegungen")});
		}],
		[400, {

			~addr1.sendMsg(\pll2Label, "FILTER");
			~addr2.sendMsg(\pll2Label, "FILTER");
			~addr3.sendMsg(\pll2Label, "FILTER");

			"Section 3".postln;
			~synthDict.at(\pll1).set(\filLag, 4);
			~synthDict.at(\pll1).set(\filSel, 2);
			//~synthDict.at(\pll1).set(\filModSpeed, 0.04);

			~controls1.replace(LBLControlXY(\xy2, [ControlSpec(0.02, 1, 2, units: "LPRQ"), ControlSpec(40, 3500, 2, units: "LPFreq")], [0.1, 900], \pll1,
				[\rq1, \fil1], ['/lpRQLabel', '/lpFreqLabel']));
			~controls2.replace(LBLControlXY(\xy2, [ControlSpec(0.02, 1, 2, units: "LPRQ"), ControlSpec(40, 3500, 2, units: "LPFreq")], [0.1, 900], \pll1,
				[\rq2, \fil2], ['/lpRQLabel', '/lpFreqLabel']));
			~controls3.replace(LBLControlXY(\xy2, [ControlSpec(0.02, 1, 2, units: "LPRQ"), ControlSpec(40, 3500, 2, units: "LPFreq")], [0.1, 900], \pll1,
				[\rq3, \fil3], ['/lpRQLabel', '/lpFreqLabel']));

			~controls1.getAndListen(\xy2);
			~controls2.getAndListen(\xy2);
			~controls3.getAndListen(\xy2);

			~controls1.get(\pllXY).func = nil;
			~controls2.get(\pllXY).func = nil;
			~controls3.get(\pllXY).func = nil;

			~controls1.setSynthUpdate(~synthDict, true);
			~controls2.setSynthUpdate(~synthDict, true);
			~controls3.setSynthUpdate(~synthDict, true);

			~synthDict.at(\pll1).set(\combAmp1, 0.1, \combAmp2, 0.1, \combAmp3, 0.1,  \combOnOff1, 0, \combOnOff2, 0, \combOnOff3, 0);

			~synthDict.at(\pll1).set(\combHighAmp, 0.3);// \fil1max, 3000, \fil2max, 3000, \fil3max, 3000, \fil1min, 30, \fil2min, 30, \fil3min, 30);

			~synthDict.at(\pll1).set(\ampLag1, 0.005, \amp1, 0.55, \ampLag2, 0.005, \amp2, 0.55, \ampLag3, 0.005, \amp3, 0.55);
			~synthDict.at(\fbpll1).set(\amp, 1);
			~synthDict.at(\fbpll2).set(\amp, 1);
			~synthDict.at(\fbpll3).set(\amp, 1);


			~controls1.setParameterList([\freq1, 900, \errFreq1, 1220, \leak1, 0.98]);
			~controls2.setParameterList([\freq1, 1800, \errFreq2, 1233, \leak2, 0.93]);
			~controls3.setParameterList([\freq1, 230, \errFreq3, 1123, \leak3, 0.91]);

			~controls1.setSpec(\freqCont, ControlSpec(0.1, 2200, 4, units: "GFK"));
			~controls2.setSpec(\freqCont, ControlSpec(0.1, 2200, 4, units: "GFK"));
			~controls3.setSpec(\freqCont, ControlSpec(0.1, 2200, 4, units: "GFK"));


			~controls1.setSpec(\pllXY, [ControlSpec(0.9, 0.98, 1, units: "Leak"), ControlSpec(15, 2500, 2, units: "Freq")]);
			~controls2.setSpec(\pllXY, [ControlSpec(0.9, 0.98, 1, units: "Leak"), ControlSpec(15, 2500, 2, units: "Freq")]);
			~controls3.setSpec(\pllXY, [ControlSpec(0.9, 0.98, 1, units: "Leak"), ControlSpec(15, 2500, 2, units: "Freq")]);

		}],
		[405, {

			~synthDict.at(\fbpll1).set(\amp, 0, \ampLag, 80);
			~synthDict.at(\fbpll2).set(\amp, 0, \ampLag, 80);
			~synthDict.at(\fbpll3).set(\amp, 0, \ampLag, 80);

		}],

		[520, {
			[~addr1, ~addr2, ~addr3].do({arg a; a.sendMsg(\score, "530 - 700\nGFD: ca. alle 40 Sek. Frequenz wechseln\nKOPPLUNG: allmählich Richtung unteren  linken Quadranten\nFILTER: wie vorher ")});
		}],

		[530, {
			"Section 4".postln;
			//~synth.set(\filLag, 4);
			~synthDict.at(\pll1).set(\pllParLag, 2);

			~synthDict.at(\pll1).set(\lpfHpf1, 0.08, \lpfHpf2, 0.08, \lpfHpf3, 0.08);

			~controls1.add(LBLControlOptions(\freqDisc, [49, 58.27, 146.83, 277.18, 830.61, 1318.5, 1760], "GFD", 0.04, \pll1,
				\freq1, {arg v, synth; Routine({
					~amp1 = ~controls1.getVal(\amp);
					synth.set(\ampLag1, 6);
					synth.set(\amp1, 0); 6.wait;
					~controls1.set(\freqCont, v);
					1.wait;
					synth.set(\amp1, ~amp1);
					6.wait;
					synth.set(\ampLag1, 0.2);
				}).play;
			}, '/freqLabel'));

			~controls2.add(LBLControlOptions(\freqDisc, [49, 58.27, 146.83, 277.18, 830.61, 1318.5, 1760], "GFD", 0.04, \pll1,
				\freq2,  {arg v, synth; Routine({
					~amp2 = ~controls2.getVal(\amp);
					synth.set(\ampLag2, 6);
					synth.set(\amp2, 0); 6.wait;
					~controls2.set(\freqCont, v);
					1.wait;
					synth.set(\amp2, ~amp2);
					6.wait;
					synth.set(\ampLag2, 0.2);
				}).play;
			}, '/freqLabel'));

			~controls3.add(LBLControlOptions(\freqDisc, [49, 58.27, 146.83, 277.18, 830.61, 1318.5, 1760], "GFD", 0.04, \pll1,
				\freq3,{arg v, synth; Routine({
					~amp3 = ~controls3.getVal(\amp);
					synth.set(\ampLag3, 6);
					synth.set(\amp3, 0); 6.wait;
					~controls3.set(\freqCont, v);
					1.wait;
					synth.set(\amp3, ~amp3);
					6.wait;
					synth.set(\ampLag3, 0.2);
				}).play;
			}, '/freqLabel'));

			~controls1.getAndListen(\freqDisc);
			~controls2.getAndListen(\freqDisc);
			~controls3.getAndListen(\freqDisc);

			~controls1.setSynthUpdate(~synthDict, true);
			~controls2.setSynthUpdate(~synthDict, true);
			~controls3.setSynthUpdate(~synthDict, true);

			~controls1.setSpec(\pllXY, [ControlSpec(0.0, 0.6, 3, units: "Leak"), ControlSpec(0, 10, 4, units: "Freq")]);
			~controls2.setSpec(\pllXY, [ControlSpec(0.0, 0.6, 3, units: "Leak"), ControlSpec(0, 10, 4, units: "Freq")]);
			~controls3.setSpec(\pllXY, [ControlSpec(0.0, 0.6, 3, units: "Leak"), ControlSpec(0, 10, 4, units: "Freq")]);
		}],

		[690, {
			[~addr1, ~addr2, ~addr3].do({arg a; a.sendMsg(\score, "700 - 820\nGFD: GFD: Nur tiefste Frequenz\nKOPPLUNG: weiter Richtung unteren linken Quadranten\nFILTER: wie vorher ")});
		}],

		[700, {
			~synthDict.at(\pll1).set(\combAmp1, 0.0, \combAmp2, 0.0, \combAmp3, 0.0,  \combOnOff1, 0, \combOnOff2, 0, \combOnOff3, 0);
			~synthDict.at(\pll1).set(\combHighAmp, 0.1);

		}],
		[810, {

			~addr1.sendMsg(\score, "820\nAMP: langsam ausblenden ");
			~addr3.sendMsg(\score, "830\nAMP: langsam ausblenden ");
			~addr2.sendMsg(\score, "840\nAMP: langsam ausblenden ");
		}]
], [~addr1, ~addr2, ~addr3], ~pieceFunc).boot;

)


~piece.start(-5, 1000);

~piece.stop

// amp 250 higher
// übergang 400
// technik pll2 und zusammenspiel
// teil 3, nicht zu glissandi-maessig
// gfk nicht bewegen
//langsam

// optimale mit 3 subs, wie ist sub dort?
// filter bw and changes

// todo
// press ready instruction
// amp 250 higher [done]
// amp 400 attack einsatz [done]
// deaktivieren gfk in 4
// base -> gfk [done]
// pads vorbereiten, lock pin, notifications, volume [done]
// rec video test
// counter for me?
// ready state [done]
// rec on wednes