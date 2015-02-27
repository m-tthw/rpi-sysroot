//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
//
// Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, distribution,
// and distribution of modified versions of this work as long as this
// notice is included.
//++

// AUTO-GENERATED-DOCS
// Do not manually add any code below this comment
// otherwise it may be removed

void MainWindow::initDocsWindow() {

  // language en

  // tutorial info
  struct help_page tutorialHelpPages[] = {
    { "1 Welcome to Sonic Pi", NULL, ":/help/tutorial_item_1.html"},
    { "   1.1 Live Coding", NULL, ":/help/tutorial_item_2.html"},
    { "   1.2 Exploring the Interface", NULL, ":/help/tutorial_item_3.html"},
    { "   1.3 Learning through Play", NULL, ":/help/tutorial_item_4.html"},
    { "2 Synths", NULL, ":/help/tutorial_item_5.html"},
    { "   2.1 Your First Beeps", NULL, ":/help/tutorial_item_6.html"},
    { "   2.2 Synth Parameters", NULL, ":/help/tutorial_item_7.html"},
    { "   2.3 Switching Synths", NULL, ":/help/tutorial_item_8.html"},
    { "   2.4 Duration with Envelopes", NULL, ":/help/tutorial_item_9.html"},
    { "3 Samples", NULL, ":/help/tutorial_item_10.html"},
    { "   3.1 Triggering Samples", NULL, ":/help/tutorial_item_11.html"},
    { "   3.2 Sample Parameters", NULL, ":/help/tutorial_item_12.html"},
    { "   3.3 Stretching Samples", NULL, ":/help/tutorial_item_13.html"},
    { "   3.4 Enveloped Samples", NULL, ":/help/tutorial_item_14.html"},
    { "   3.5 Partial Samples", NULL, ":/help/tutorial_item_15.html"},
    { "   3.6 External Samples", NULL, ":/help/tutorial_item_16.html"},
    { "4 Randomisation", NULL, ":/help/tutorial_item_17.html"},
    { "5 Programming Structures", NULL, ":/help/tutorial_item_18.html"},
    { "   5.1 Blocks", NULL, ":/help/tutorial_item_19.html"},
    { "   5.2 Iteration and Loops", NULL, ":/help/tutorial_item_20.html"},
    { "   5.3 Conditionals", NULL, ":/help/tutorial_item_21.html"},
    { "   5.4 Threads", NULL, ":/help/tutorial_item_22.html"},
    { "   5.5 Functions", NULL, ":/help/tutorial_item_23.html"},
    { "   5.6 Variables", NULL, ":/help/tutorial_item_24.html"},
    { "   5.7 Thread Synchronisation", NULL, ":/help/tutorial_item_25.html"},
    { "6 FX", NULL, ":/help/tutorial_item_26.html"},
    { "   6.1 Adding FX", NULL, ":/help/tutorial_item_27.html"},
    { "   6.2 FX in Practice", NULL, ":/help/tutorial_item_28.html"},
    { "7 Control", NULL, ":/help/tutorial_item_29.html"},
    { "   7.1 Controlling Running Synths", NULL, ":/help/tutorial_item_30.html"},
    { "   7.2 Controlling FX", NULL, ":/help/tutorial_item_31.html"},
    { "   7.3 Sliding Parameters", NULL, ":/help/tutorial_item_32.html"},
    { "8 Data Structures", NULL, ":/help/tutorial_item_33.html"},
    { "   8.1 Lists", NULL, ":/help/tutorial_item_34.html"},
    { "   8.2 Chords", NULL, ":/help/tutorial_item_35.html"},
    { "   8.3 Scales", NULL, ":/help/tutorial_item_36.html"},
    { "   8.4 Rings", NULL, ":/help/tutorial_item_37.html"},
    { "9 Live Coding", NULL, ":/help/tutorial_item_38.html"},
    { "   9.1 Live Coding Fundamentals", NULL, ":/help/tutorial_item_39.html"},
    { "   9.2 Live Loops", NULL, ":/help/tutorial_item_40.html"},
    { "   9.3 Multiple Live Loops", NULL, ":/help/tutorial_item_41.html"},
    { "10 Essential Knowledge", NULL, ":/help/tutorial_item_42.html"},
    { "   10.1 Using Shortcuts", NULL, ":/help/tutorial_item_43.html"},
    { "   10.2 Shortcut Cheatsheet", NULL, ":/help/tutorial_item_44.html"},
    { "   10.3 Sharing", NULL, ":/help/tutorial_item_45.html"},
    { "   10.4 Performing", NULL, ":/help/tutorial_item_46.html"},
    { "11 Conclusions", NULL, ":/help/tutorial_item_47.html"},
  };

  addHelpPage(createHelpTab(tr("Tutorial")), tutorialHelpPages, 47);


  // examples info
  struct help_page examplesHelpPages[] = {
    { "[Apprentice] Haunted Bells", NULL, ":/help/examples_item_48.html"},
    { "[Illusionist] Filtered Dnb", NULL, ":/help/examples_item_49.html"},
    { "[Illusionist] Ocean", NULL, ":/help/examples_item_50.html"},
    { "[Illusionist] Fm Noise", NULL, ":/help/examples_item_51.html"},
    { "[Illusionist] Jungle", NULL, ":/help/examples_item_52.html"},
    { "[Magician] Idm Breakbeat", NULL, ":/help/examples_item_53.html"},
    { "[Magician] Acid", NULL, ":/help/examples_item_54.html"},
    { "[Magician] Wob Rhyth", NULL, ":/help/examples_item_55.html"},
    { "[Magician] Echo Drama", NULL, ":/help/examples_item_56.html"},
    { "[Magician] Ambient", NULL, ":/help/examples_item_57.html"},
    { "[Magician] Compus Beats", NULL, ":/help/examples_item_58.html"},
    { "[Magician] Tron Bike", NULL, ":/help/examples_item_59.html"},
    { "[Sorcerer] Driving Pulse", NULL, ":/help/examples_item_60.html"},
    { "[Sorcerer] Monday Blues", NULL, ":/help/examples_item_61.html"},
    { "[Sorcerer] Square Skit", NULL, ":/help/examples_item_62.html"},
    { "[Sorcerer] Bach", NULL, ":/help/examples_item_63.html"},
    { "[Wizard] Blip Rhythm", NULL, ":/help/examples_item_64.html"},
    { "[Wizard] Blimp Zones", NULL, ":/help/examples_item_65.html"},
    { "[Wizard] Time Machine", NULL, ":/help/examples_item_66.html"},
    { "[Wizard] Tilburg", NULL, ":/help/examples_item_67.html"},
    { "[Wizard] Shufflit", NULL, ":/help/examples_item_68.html"},
    { "[Algomancer] Sonic Dreams", NULL, ":/help/examples_item_69.html"},
  };

  addHelpPage(createHelpTab(tr("Examples")), examplesHelpPages, 22);


  // synths info
  struct help_page synthsHelpPages[] = {
    { "Beep", "beep", ":/help/synths_item_70.html"},
    { "Bnoise", "bnoise", ":/help/synths_item_71.html"},
    { "Cnoise", "cnoise", ":/help/synths_item_72.html"},
    { "Dark Ambience", "dark_ambience", ":/help/synths_item_73.html"},
    { "Dsaw", "dsaw", ":/help/synths_item_74.html"},
    { "Dull Bell", "dull_bell", ":/help/synths_item_75.html"},
    { "Fm", "fm", ":/help/synths_item_76.html"},
    { "Gnoise", "gnoise", ":/help/synths_item_77.html"},
    { "Growl", "growl", ":/help/synths_item_78.html"},
    { "Hollow", "hollow", ":/help/synths_item_79.html"},
    { "Mod Beep", "mod_beep", ":/help/synths_item_80.html"},
    { "Mod Dsaw", "mod_dsaw", ":/help/synths_item_81.html"},
    { "Mod Fm", "mod_fm", ":/help/synths_item_82.html"},
    { "Mod Pulse", "mod_pulse", ":/help/synths_item_83.html"},
    { "Mod Saw", "mod_saw", ":/help/synths_item_84.html"},
    { "Mod Sine", "mod_sine", ":/help/synths_item_85.html"},
    { "Mod Tri", "mod_tri", ":/help/synths_item_86.html"},
    { "Noise", "noise", ":/help/synths_item_87.html"},
    { "Pnoise", "pnoise", ":/help/synths_item_88.html"},
    { "Pretty Bell", "pretty_bell", ":/help/synths_item_89.html"},
    { "Prophet", "prophet", ":/help/synths_item_90.html"},
    { "Pulse", "pulse", ":/help/synths_item_91.html"},
    { "Saw", "saw", ":/help/synths_item_92.html"},
    { "Sine", "sine", ":/help/synths_item_93.html"},
    { "Square", "square", ":/help/synths_item_94.html"},
    { "Supersaw", "supersaw", ":/help/synths_item_95.html"},
    { "Tb303", "tb303", ":/help/synths_item_96.html"},
    { "Tri", "tri", ":/help/synths_item_97.html"},
    { "Zawa", "zawa", ":/help/synths_item_98.html"},
  };

  addHelpPage(createHelpTab(tr("Synths")), synthsHelpPages, 29);


  // fx info
  struct help_page fxHelpPages[] = {
    { "Bitcrusher", "bitcrusher", ":/help/fx_item_99.html"},
    { "BPF", "bpf", ":/help/fx_item_100.html"},
    { "Compressor", "compressor", ":/help/fx_item_101.html"},
    { "Distortion", "distortion", ":/help/fx_item_102.html"},
    { "Echo", "echo", ":/help/fx_item_103.html"},
    { "Flanger", "flanger", ":/help/fx_item_104.html"},
    { "HPF", "hpf", ":/help/fx_item_105.html"},
    { "Ixi Techno", "ixi_techno", ":/help/fx_item_106.html"},
    { "Level", "level", ":/help/fx_item_107.html"},
    { "LPF", "lpf", ":/help/fx_item_108.html"},
    { "NBPF", "nbpf", ":/help/fx_item_109.html"},
    { "NHPF", "nhpf", ":/help/fx_item_110.html"},
    { "NLPF", "nlpf", ":/help/fx_item_111.html"},
    { "Normaliser", "normaliser", ":/help/fx_item_112.html"},
    { "NRBPF", "nrbpf", ":/help/fx_item_113.html"},
    { "NRHPF", "nrhpf", ":/help/fx_item_114.html"},
    { "NRLPF", "nrlpf", ":/help/fx_item_115.html"},
    { "Pan", "pan", ":/help/fx_item_116.html"},
    { "RBPF", "rbpf", ":/help/fx_item_117.html"},
    { "Reverb", "reverb", ":/help/fx_item_118.html"},
    { "RHPF", "rhpf", ":/help/fx_item_119.html"},
    { "Ring", "ring", ":/help/fx_item_120.html"},
    { "RLPF", "rlpf", ":/help/fx_item_121.html"},
    { "Slicer", "slicer", ":/help/fx_item_122.html"},
    { "Wobble", "wobble", ":/help/fx_item_123.html"},
  };

  addHelpPage(createHelpTab(tr("Fx")), fxHelpPages, 25);


  // samples info
  struct help_page samplesHelpPages[] = {
    { "Ambient Sounds", NULL, ":/help/samples_item_124.html"},
    { "Bass Drums", NULL, ":/help/samples_item_125.html"},
    { "Bass Sounds", NULL, ":/help/samples_item_126.html"},
    { "Drum Sounds", NULL, ":/help/samples_item_127.html"},
    { "Electric Sounds", NULL, ":/help/samples_item_128.html"},
    { "Miscellaneous Sounds", NULL, ":/help/samples_item_129.html"},
    { "Percussive Sounds", NULL, ":/help/samples_item_130.html"},
    { "Snare Drums", NULL, ":/help/samples_item_131.html"},
    { "Sounds featuring guitars", NULL, ":/help/samples_item_132.html"},
    { "Sounds for Looping", NULL, ":/help/samples_item_133.html"},
  };

  addHelpPage(createHelpTab(tr("Samples")), samplesHelpPages, 10);


  // lang info
  struct help_page langHelpPages[] = {
    { "all_sample_names", "all_sample_names", ":/help/lang_item_134.html"},
    { "at", "at", ":/help/lang_item_135.html"},
    { "bools", "bools", ":/help/lang_item_136.html"},
    { "choose", "choose", ":/help/lang_item_137.html"},
    { "chord", "chord", ":/help/lang_item_138.html"},
    { "chord_degree", "chord_degree", ":/help/lang_item_139.html"},
    { "comment", "comment", ":/help/lang_item_140.html"},
    { "control", "control", ":/help/lang_item_141.html"},
    { "cue", "cue", ":/help/lang_item_142.html"},
    { "current_arg_checks", "current_arg_checks", ":/help/lang_item_143.html"},
    { "current_bpm", "current_bpm", ":/help/lang_item_144.html"},
    { "current_debug", "current_debug", ":/help/lang_item_145.html"},
    { "current_sample_pack", "current_sample_pack", ":/help/lang_item_146.html"},
    { "current_sample_pack_aliases", "current_sample_pack_aliases", ":/help/lang_item_147.html"},
    { "current_sched_ahead_time", "current_sched_ahead_time", ":/help/lang_item_148.html"},
    { "current_synth", "current_synth", ":/help/lang_item_149.html"},
    { "current_synth_defaults", "current_synth_defaults", ":/help/lang_item_150.html"},
    { "current_transpose", "current_transpose", ":/help/lang_item_151.html"},
    { "current_volume", "current_volume", ":/help/lang_item_152.html"},
    { "dec", "dec", ":/help/lang_item_153.html"},
    { "define", "define", ":/help/lang_item_154.html"},
    { "defonce", "defonce", ":/help/lang_item_155.html"},
    { "degree", "degree", ":/help/lang_item_156.html"},
    { "density", "density", ":/help/lang_item_157.html"},
    { "dice", "dice", ":/help/lang_item_158.html"},
    { "factor?", "factor?", ":/help/lang_item_159.html"},
    { "hz_to_midi", "hz_to_midi", ":/help/lang_item_160.html"},
    { "in_thread", "in_thread", ":/help/lang_item_161.html"},
    { "inc", "inc", ":/help/lang_item_162.html"},
    { "knit", "knit", ":/help/lang_item_163.html"},
    { "live_loop", "live_loop", ":/help/lang_item_164.html"},
    { "load_sample", "load_sample", ":/help/lang_item_165.html"},
    { "load_samples", "load_samples", ":/help/lang_item_166.html"},
    { "load_synthdefs", "load_synthdefs", ":/help/lang_item_167.html"},
    { "midi_to_hz", "midi_to_hz", ":/help/lang_item_168.html"},
    { "ndefine", "ndefine", ":/help/lang_item_169.html"},
    { "note", "note", ":/help/lang_item_170.html"},
    { "note_info", "note_info", ":/help/lang_item_171.html"},
    { "one_in", "one_in", ":/help/lang_item_172.html"},
    { "play", "play", ":/help/lang_item_173.html"},
    { "play_chord", "play_chord", ":/help/lang_item_174.html"},
    { "play_pattern", "play_pattern", ":/help/lang_item_175.html"},
    { "play_pattern_timed", "play_pattern_timed", ":/help/lang_item_176.html"},
    { "print", "print", ":/help/lang_item_177.html"},
    { "puts", "puts", ":/help/lang_item_178.html"},
    { "quantise", "quantise", ":/help/lang_item_179.html"},
    { "rand", "rand", ":/help/lang_item_180.html"},
    { "rand_i", "rand_i", ":/help/lang_item_181.html"},
    { "range", "range", ":/help/lang_item_182.html"},
    { "rdist", "rdist", ":/help/lang_item_183.html"},
    { "rest?", "rest?", ":/help/lang_item_184.html"},
    { "ring", "ring", ":/help/lang_item_185.html"},
    { "rrand", "rrand", ":/help/lang_item_186.html"},
    { "rrand_i", "rrand_i", ":/help/lang_item_187.html"},
    { "rt", "rt", ":/help/lang_item_188.html"},
    { "sample", "sample", ":/help/lang_item_189.html"},
    { "sample_buffer", "sample_buffer", ":/help/lang_item_190.html"},
    { "sample_duration", "sample_duration", ":/help/lang_item_191.html"},
    { "sample_groups", "sample_groups", ":/help/lang_item_192.html"},
    { "sample_info", "sample_info", ":/help/lang_item_193.html"},
    { "sample_loaded?", "sample_loaded?", ":/help/lang_item_194.html"},
    { "sample_names", "sample_names", ":/help/lang_item_195.html"},
    { "scale", "scale", ":/help/lang_item_196.html"},
    { "set_control_delta!", "set_control_delta!", ":/help/lang_item_197.html"},
    { "set_sched_ahead_time!", "set_sched_ahead_time!", ":/help/lang_item_198.html"},
    { "set_volume!", "set_volume!", ":/help/lang_item_199.html"},
    { "shuffle", "shuffle", ":/help/lang_item_200.html"},
    { "sleep", "sleep", ":/help/lang_item_201.html"},
    { "spread", "spread", ":/help/lang_item_202.html"},
    { "status", "status", ":/help/lang_item_203.html"},
    { "stop", "stop", ":/help/lang_item_204.html"},
    { "sync", "sync", ":/help/lang_item_205.html"},
    { "synth", "synth", ":/help/lang_item_206.html"},
    { "uncomment", "uncomment", ":/help/lang_item_207.html"},
    { "use_arg_bpm_scaling", "use_arg_bpm_scaling", ":/help/lang_item_208.html"},
    { "use_arg_checks", "use_arg_checks", ":/help/lang_item_209.html"},
    { "use_bpm", "use_bpm", ":/help/lang_item_210.html"},
    { "use_bpm_mul", "use_bpm_mul", ":/help/lang_item_211.html"},
    { "use_debug", "use_debug", ":/help/lang_item_212.html"},
    { "use_merged_synth_defaults", "use_merged_synth_defaults", ":/help/lang_item_213.html"},
    { "use_random_seed", "use_random_seed", ":/help/lang_item_214.html"},
    { "use_sample_pack", "use_sample_pack", ":/help/lang_item_215.html"},
    { "use_sample_pack_as", "use_sample_pack_as", ":/help/lang_item_216.html"},
    { "use_synth", "use_synth", ":/help/lang_item_217.html"},
    { "use_synth_defaults", "use_synth_defaults", ":/help/lang_item_218.html"},
    { "use_transpose", "use_transpose", ":/help/lang_item_219.html"},
    { "version", "version", ":/help/lang_item_220.html"},
    { "vt", "vt", ":/help/lang_item_221.html"},
    { "wait", "wait", ":/help/lang_item_222.html"},
    { "with_arg_bpm_scaling", "with_arg_bpm_scaling", ":/help/lang_item_223.html"},
    { "with_arg_checks", "with_arg_checks", ":/help/lang_item_224.html"},
    { "with_bpm", "with_bpm", ":/help/lang_item_225.html"},
    { "with_bpm_mul", "with_bpm_mul", ":/help/lang_item_226.html"},
    { "with_debug", "with_debug", ":/help/lang_item_227.html"},
    { "with_fx", "with_fx", ":/help/lang_item_228.html"},
    { "with_merged_synth_defaults", "with_merged_synth_defaults", ":/help/lang_item_229.html"},
    { "with_random_seed", "with_random_seed", ":/help/lang_item_230.html"},
    { "with_sample_pack", "with_sample_pack", ":/help/lang_item_231.html"},
    { "with_sample_pack_as", "with_sample_pack_as", ":/help/lang_item_232.html"},
    { "with_synth", "with_synth", ":/help/lang_item_233.html"},
    { "with_synth_defaults", "with_synth_defaults", ":/help/lang_item_234.html"},
    { "with_transpose", "with_transpose", ":/help/lang_item_235.html"},
  };

  addHelpPage(createHelpTab(tr("Lang")), langHelpPages, 102);

  // FX arguments for autocompletion
  QStringList fxtmp;
  // fx :bitcrusher
  fxtmp.clear(); fxtmp << "amp:" << "mix:" << "pre_amp:" << "sample_rate:" << "bits:" ;
  autocomplete->addFXArgs(":bitcrusher", fxtmp);

  // fx :reverb
  fxtmp.clear(); fxtmp << "amp:" << "mix:" << "pre_amp:" << "room:" << "damp:" ;
  autocomplete->addFXArgs(":reverb", fxtmp);

  // fx :level
  fxtmp.clear(); fxtmp << "amp:" ;
  autocomplete->addFXArgs(":level", fxtmp);

  // fx :echo
  fxtmp.clear(); fxtmp << "amp:" << "mix:" << "pre_amp:" << "phase:" << "decay:" << "max_phase:" ;
  autocomplete->addFXArgs(":echo", fxtmp);

  // fx :slicer
  fxtmp.clear(); fxtmp << "amp:" << "mix:" << "pre_amp:" << "phase:" << "amp_min:" << "amp_max:" << "pulse_width:" << "phase_offset:" << "wave:" << "invert_wave:" ;
  autocomplete->addFXArgs(":slicer", fxtmp);

  // fx :wobble
  fxtmp.clear(); fxtmp << "amp:" << "mix:" << "pre_amp:" << "phase:" << "cutoff_min:" << "cutoff_max:" << "res:" << "phase_offset:" << "wave:" << "pulse_width:" << "filter:" ;
  autocomplete->addFXArgs(":wobble", fxtmp);

  // fx :ixi_techno
  fxtmp.clear(); fxtmp << "amp:" << "mix:" << "pre_amp:" << "phase:" << "phase_offset:" << "cutoff_min:" << "cutoff_max:" << "res:" ;
  autocomplete->addFXArgs(":ixi_techno", fxtmp);

  // fx :compressor
  fxtmp.clear(); fxtmp << "amp:" << "mix:" << "pre_amp:" << "threshold:" << "clamp_time:" << "slope_above:" << "slope_below:" << "relax_time:" ;
  autocomplete->addFXArgs(":compressor", fxtmp);

  // fx :rlpf
  fxtmp.clear(); fxtmp << "amp:" << "mix:" << "pre_amp:" << "cutoff:" << "res:" ;
  autocomplete->addFXArgs(":rlpf", fxtmp);

  // fx :nrlpf
  fxtmp.clear(); fxtmp << "amp:" << "mix:" << "pre_amp:" << "cutoff:" << "res:" ;
  autocomplete->addFXArgs(":nrlpf", fxtmp);

  // fx :rhpf
  fxtmp.clear(); fxtmp << "amp:" << "mix:" << "pre_amp:" << "cutoff:" << "res:" ;
  autocomplete->addFXArgs(":rhpf", fxtmp);

  // fx :nrhpf
  fxtmp.clear(); fxtmp << "amp:" << "mix:" << "pre_amp:" << "cutoff:" << "res:" ;
  autocomplete->addFXArgs(":nrhpf", fxtmp);

  // fx :hpf
  fxtmp.clear(); fxtmp << "amp:" << "mix:" << "pre_amp:" << "cutoff:" ;
  autocomplete->addFXArgs(":hpf", fxtmp);

  // fx :nhpf
  fxtmp.clear(); fxtmp << "amp:" << "mix:" << "pre_amp:" << "cutoff:" << "res:" ;
  autocomplete->addFXArgs(":nhpf", fxtmp);

  // fx :lpf
  fxtmp.clear(); fxtmp << "amp:" << "mix:" << "pre_amp:" << "cutoff:" ;
  autocomplete->addFXArgs(":lpf", fxtmp);

  // fx :nlpf
  fxtmp.clear(); fxtmp << "amp:" << "mix:" << "pre_amp:" << "cutoff:" ;
  autocomplete->addFXArgs(":nlpf", fxtmp);

  // fx :normaliser
  fxtmp.clear(); fxtmp << "amp:" << "mix:" << "pre_amp:" << "level:" ;
  autocomplete->addFXArgs(":normaliser", fxtmp);

  // fx :distortion
  fxtmp.clear(); fxtmp << "amp:" << "mix:" << "pre_amp:" << "distort:" ;
  autocomplete->addFXArgs(":distortion", fxtmp);

  // fx :pan
  fxtmp.clear(); fxtmp << "amp:" << "mix:" << "pre_amp:" << "pan:" ;
  autocomplete->addFXArgs(":pan", fxtmp);

  // fx :bpf
  fxtmp.clear(); fxtmp << "amp:" << "mix:" << "pre_amp:" << "centre:" << "res:" ;
  autocomplete->addFXArgs(":bpf", fxtmp);

  // fx :nbpf
  fxtmp.clear(); fxtmp << "amp:" << "mix:" << "pre_amp:" << "centre:" << "res:" ;
  autocomplete->addFXArgs(":nbpf", fxtmp);

  // fx :rbpf
  fxtmp.clear(); fxtmp << "amp:" << "mix:" << "pre_amp:" << "centre:" << "res:" ;
  autocomplete->addFXArgs(":rbpf", fxtmp);

  // fx :nrbpf
  fxtmp.clear(); fxtmp << "amp:" << "mix:" << "pre_amp:" << "centre:" << "res:" ;
  autocomplete->addFXArgs(":nrbpf", fxtmp);

  // fx :ring
  fxtmp.clear(); fxtmp << "freq:" << "amp:" << "mix:" << "pre_amp:" << "mod_amp:" ;
  autocomplete->addFXArgs(":ring", fxtmp);

  // fx :flanger
  fxtmp.clear(); fxtmp << "amp:" << "mix:" << "pre_amp:" << "phase:" << "phase_offset:" << "wave:" << "invert_wave:" << "stereo_invert_wave:" << "delay:" << "max_delay:" << "depth:" << "decay:" << "feedback:" << "invert_flange:" ;
  autocomplete->addFXArgs(":flanger", fxtmp);

}
