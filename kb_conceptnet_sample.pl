% Auto-generated KB
entity(concept_squat, concept, 'squat').
entity(concept_exercise, concept, 'exercise').
fact(concept_squat, rel_isa, concept_exercise).
confidence(fact(concept_squat, rel_isa, concept_exercise), 'conceptnet', 0.0).
entity(concept_push_up, concept, 'push up').
entity(concept_exercise, concept, 'exercise').
fact(concept_push_up, rel_isa, concept_exercise).
confidence(fact(concept_push_up, rel_isa, concept_exercise), 'conceptnet', 0.0).
entity(concept_leg_press, concept, 'leg press').
entity(concept_exercise, concept, 'exercise').
fact(concept_leg_press, rel_isa, concept_exercise).
confidence(fact(concept_leg_press, rel_isa, concept_exercise), 'conceptnet', 0.0).
entity(concept_squat, concept, 'squat').
entity(concept_quadriceps, concept, 'quadriceps').
fact(concept_squat, rel_uses, concept_quadriceps).
confidence(fact(concept_squat, rel_uses, concept_quadriceps), 'conceptnet', 0.0).
entity(concept_squat, concept, 'squat').
entity(concept_glutes, concept, 'glutes').
fact(concept_squat, rel_uses, concept_glutes).
confidence(fact(concept_squat, rel_uses, concept_glutes), 'conceptnet', 0.0).
entity(concept_box_squat, concept, 'box squat').
entity(concept_squat, concept, 'squat').
fact(concept_box_squat, rel_relatedto, concept_squat).
confidence(fact(concept_box_squat, rel_relatedto, concept_squat), 'conceptnet', 0.0).
entity(concept_leg_press, concept, 'leg press').
entity(concept_squat, concept, 'squat').
fact(concept_leg_press, rel_substitutefor, concept_squat).
confidence(fact(concept_leg_press, rel_substitutefor, concept_squat), 'conceptnet', 0.0).
entity(concept_jump_rope, concept, 'jump rope').
entity(concept_cardio, concept, 'cardio').
fact(concept_jump_rope, rel_isa, concept_cardio).
confidence(fact(concept_jump_rope, rel_isa, concept_cardio), 'conceptnet', 0.0).
entity(concept_glute_bridge, concept, 'glute bridge').
entity(concept_glutes, concept, 'glutes').
fact(concept_glute_bridge, rel_uses, concept_glutes).
confidence(fact(concept_glute_bridge, rel_uses, concept_glutes), 'conceptnet', 0.0).
entity(concept_squat, concept, 'squat').
entity(concept_knee_injury, concept, 'knee injury').
fact(concept_squat, rel_contraindicatedfor, concept_knee_injury).
confidence(fact(concept_squat, rel_contraindicatedfor, concept_knee_injury), 'conceptnet', 0.0).
