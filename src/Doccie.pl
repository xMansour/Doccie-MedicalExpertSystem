:- use_module(library(jpl)).    %Importing the jpl interface which connects the prolog and java together.


start :-sleep(0.4),	            %Makes a delay of 0.4 seconds
		
		write("Running"),nl,nl,			%Writes a text to the consol		
		interface2.  			%Executing the interface2 which is responsable for taking the name




%These are the rules of the system

    symptom(Patient,fever) :- verify(Patient," have a fever (y/n) ?").		%symptom is a 3rd		verify is the a method in line 98						   		
 
    symptom(Patient,rash) :- verify(Patient," have a rash (y/n) ?").	
  
    symptom(Patient,headache) :- verify(Patient," have a headache (y/n) ?").

    symptom(Patient,runny_nose) :- verify(Patient," have a runny nose (y/n) ?").
    
    symptom(Patient,conjunctivitis) :- verify(Patient," have a conjunctivitis (y/n) ?").
    
    symptom(Patient,cough) :- verify(Patient," have a cough (y/n) ?").
	
    symptom(Patient,body_ache) :- verify(Patient," have a body ache (y/n) ?").
 
    symptom(Patient,chills) :- verify(Patient," have a chills (y/n) ?").
   
    symptom(Patient,sore_throat) :- verify(Patient," have a sore throat (y/n) ?").
  
    symptom(Patient,sneezing) :- verify(Patient," have a sneezing (y/n) ?").

    symptom(Patient,wheezes) :- verify(Patient," have a wheezes (y/n) ?").

    symptom(Patient,dyspnya) :- verify(Patient," have a dyspnya (y/n) ?").

    symptom(Patient,dizziness) :- verify(Patient," have a dizziness (y/n) ?").

    symptom(Patient,tooth_pain) :- verify(Patient," have a tooth pain (y/n) ?").

    symptom(Patient,couse_air_infection) :- verify(Patient," have a couse air infection (y/n) ?").

    symptom(Patient,otitis_media) :- verify(Patient," have a otitis media (y/n) ?").

    symptom(Patient,sinusities) :- verify(Patient," have a sinusities (y/n) ?").
   
    symptom(Patient,swollen_glands) :- verify(Patient," have a swollen glands (y/n) ?").
	
	

%These are the rules of the system based on the verified symptoms

    hypothesis(Patient,measles) :-					
        symptom(Patient,fever),
        symptom(Patient,cough),
        symptom(Patient,conjunctivitis),
        symptom(Patient,runny_nose),
        symptom(Patient,rash).
    
    hypothesis(Patient,german_measles) :-
        symptom(Patient,fever),
        symptom(Patient,headache),
        symptom(Patient,runny_nose),
        symptom(Patient,rash).
        
    hypothesis(Patient,flu) :-
        symptom(Patient,fever),
        symptom(Patient,headache),
        symptom(Patient,body_ache),
        symptom(Patient,conjunctivitis),
        symptom(Patient,chills),
        symptom(Patient,sore_throat),
        symptom(Patient,runny_nose),
        symptom(Patient,cough).    
        
    hypothesis(Patient,common_cold) :-
        symptom(Patient,headache),
        symptom(Patient,sneezing),
        symptom(Patient,sore_throat),
        symptom(Patient,runny_nose),
        symptom(Patient,chills).
        
    hypothesis(Patient,mumps) :-
        symptom(Patient,fever),
        symptom(Patient,swollen_glands).
    
    hypothesis(Patient,chicken_pox) :-
        symptom(Patient,fever),
        symptom(Patient,chills),
        symptom(Patient,body_ache),
        symptom(Patient,rash).
    
    hypothesis(Patient,measles) :-
        symptom(Patient,cough),
        symptom(Patient,sneezing),
        symptom(Patient,runny_nose).

    hypothesis(Patient,acute_asthma) :-
        symptom(Patient,cough),
        symptom(Patient,wheezes),
        symptom(Patient,dyspnya).

       
    hypothesis(Patient,hypotension) :-
        symptom(Patient,dizziness),
        symptom(Patient,headache).    
        

    hypothesis(Patient,headache) :-
        symptom(Patient,tooth_pain),
        symptom(Patient,couse_air_infection),
        symptom(Patient,otitis_media),
        symptom(Patient,sinusities).  


	hypothesis(_,"disease. It isn't within my knowledge base yet").
	
    response(Reply) :-
        read(Reply),
        write(Reply),nl.
		
ask(Patient,Question) :-
	write(Patient),write(', do you'),write(Question),	
	interface(', do you',Patient,Question),nl.
	
:- dynamic yes/1,no/1.		
	
verify(P,S) :-
   (yes(S) 
    ->
    true ;
    (no(S)
     ->
     fail ;
     ask(P,S))).
	 
undo :- retract(yes(_)),fail. 
undo :- retract(no(_)),fail.
undo.


pt(Patient):- 

		hypothesis(Patient,Disease),
		interface3(Patient,', you probably have ',Disease,'.'),
        write(Patient),write(', you probably have '),write(Disease),write('.'),nl,nl,
desease_description(Disease),
undo,end.



		

		
 		

end :-
		nl,nl,nl,
		sleep(0.4),
		write("Shutting Down"),nl.

interface(X,Y,Z) :-			%Asks the Questions
	atom_concat(Y,X, FAtom),  %atom_concat(?Atom1, ?Atom2, ?Atom3)  Atom3 forms the concatenation of Atom1 and Atom2.
	atom_concat(FAtom,Z,FinalAtom),
	jpl_new('javax.swing.JFrame', ['Doccie'], F),									%F is the frame
	jpl_new('javax.swing.JLabel',['Medical Expert System'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [400,300], _),
	jpl_call(F, setSize, [400,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showInputDialog, [F,FinalAtom], N),						%N will hold either y or n		
	jpl_call(F, dispose, [], _), 
	write(N),nl,
	( (N == yes ; N == y)
      ->
       assert(yes(Z)) ;
       assert(no(Z)), fail).
	   		
interface2 :-				%Takes the name
	jpl_new('javax.swing.JFrame', ['Doccie'], F),
	jpl_new('javax.swing.JLabel',['Medical Expert System'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [400,300], _),
	jpl_call(F, setSize, [400,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showInputDialog, [F,'Hi. How are you? Please tell me your name'], N),		%N will hold the entered name
	jpl_call(F, dispose, [], _), 
	/*write(N),nl,*/
	(	N == @(null)				%null only happens when we cancel button is clicked, if nothing was entered it will be considered empty String ""
		->	write('You cancelled'),interface3('You cancelled. ','Thank you ','for useing ','Doccie.'),end,fail
		;	write("Hi. How are you? Please tell me your name : "),write(N),nl,pt(N)
	).
	
	
interface3(P,W1,D,W2) :-	%Tell the Patient whats his illness			
	atom_concat(P,W1, A),    
	atom_concat(A,D,B),
	atom_concat(B,W2,W3),
	jpl_new('javax.swing.JFrame', ['Doccie'], F),
	jpl_new('javax.swing.JLabel',['Medical Expert System'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [400,300], _),
	jpl_call(F, setSize, [400,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showMessageDialog, [F,W3], N),			%F is the frame			W3 is the message
	jpl_call(F, dispose, [], _), 
	/*write(N),nl,*/
	(	N == @(void)			%Exeption handling
		->	write('')
		;	write("")

	).


desease_description(Disease):-

  Disease == 'measles',
  write('Measles is a childhood infection caused by a virus. Once quite common, measles can now almost always be prevented with a vaccine. Signs and symptoms of measles include cough, runny nose, inflamed eyes, sore throat, fever and a red, blotchy skin rash. we recommend acetaminophen to relieve fever and muscle aches, rest to help boost your immune system,plenty of fluids (six to eight glasses of water a day),humidifier to ease a cough and sore throat,vitamin A supplements ');
 
  Disease == 'german_measles',
  write('German measles, also known as rubella, is a viral infection that causes a red rash on the body. Aside from the rash, people with German measles usually have a fever and swollen lymph nodes. The infection can spread from person to person through contact with droplets from an infected persons sneeze or cough. Most cases of German measles are treated at home. Your doctor may tell you to rest in bed and to take acetaminophen (Tylenol), which can help relieve discomfort from fever and aches. They may also recommend that you stay home from work or school to prevent spreading the virus to others.');
 
  Disease == 'flu',
  write('Influenza, commonly known as "the flu", is an infectious disease caused by an influenza virus. Symptoms can be mild to severe. The most common symptoms include: a high fever, runny nose, sore throat, muscle pains, headache, coughing, and feeling tired. People with the flu are advised to get plenty of rest, drink plenty of liquids, avoid using alcohol and tobacco and, if necessary, take medications such as acetaminophen (paracetamol) to relieve the fever and muscle aches associated with the flu');
 
  Disease == 'common_cold',
  write('The common cold, also known simply as a cold, is a viral infectious disease of the upper respiratory tract that primarily affects the nose. The throat, sinuses, and larynx may also be affected. Signs and symptoms may appear less than two days after exposure to the virus. These may include coughing, sore throat, runny nose, sneezing, headache, and fever. Treatment for the common cold primarily involves medications and other therapies for symptomatic relief. Getting plenty of rest, drinking fluids to maintain hydration, and gargling with warm salt water are reasonable conservative measures. Much of the benefit from symptomatic treatment is, however, attributed to the placebo effect. No medications or herbal remedies have been conclusively demonstrated to shorten the duration of infection.');
 
  Disease == 'mumps',
  write('Mumps is a viral disease caused by the mumps virus. Initial signs and symptoms often include fever, muscle pain, headache, poor appetite, and feeling tired. This is then usually followed by painful swelling of one or both parotid salivary glands.Treatment for mumps. There is no treatment for mumps itself, but age-appropriate painkillers, such as paracetamol or ibuprofen may help relieve some of the symptoms. A cold compress such as a moist flannel may help relieve some of the pain from the swollen glands.');
 
  Disease == 'chicken_pox',
  write('Chickenpox, also known as varicella, is a highly contagious disease caused by the initial infection with varicella zoster virus (VZV). The disease results in a characteristic skin rash that forms small, itchy blisters, which eventually scab over. It usually starts on the chest, back, and face then spreads to the rest of the body. Acyclovir, an antiviral medication, is licensed for treatment of chickenpox. The medication works best if it is given within the first 24 hours after the rash starts. For more information, see Acyclovir Treatment. Other antiviral medications that may also work against chickenpox include valacyclovir and famciclovir.');
  
  Disease == 'acute_asthma',
  write('An acute asthma exacerbation is commonly referred to as an asthma attack. The classic symptoms are shortness of breath, wheezing, and chest tightness. The wheezing is most often when breathing out. Treatment of rapidly worsening symptoms is usually with an inhaled short-acting beta-2 agonist such as salbutamol and corticosteroids taken by mouth. In very severe cases, intravenous corticosteroids, magnesium sulfate, and hospitalization may be required.');

  Disease == 'hypotension',
  write('Hypotension is low blood pressure, especially in the arteries of the systemic circulation. Blood pressure is the force of blood pushing against the walls of the arteries as the heart pumps out blood. You should use more salt. Experts usually recommend limiting salt in your diet because sodium can raise blood pressure, sometimes dramatically. Drink more water. Fluids increase blood volume and help prevent dehydration, both of which are important in treating hypotension. Wear compression stockings. Medications.');

  Disease == 'headache',
  write('Headache is the symptom of pain anywhere in the region of the head or neck. It occurs in migraines (sharp, or throbbing pains), tension-type headaches, and cluster headaches. Frequent headaches can affect relationships and employment. There is also an increased risk of depression in those with severe headaches. Rest in a quiet, dark room. Hot or cold compresses to your head or neck. Massage and small amounts of caffeine. Over-the-counter medications such as ibuprofen (Advil, Motrin IB, others), acetaminophen (Tylenol, others), and aspirin.');

desease_description(_).


help :- write("To start type 'start.' and press Enter").