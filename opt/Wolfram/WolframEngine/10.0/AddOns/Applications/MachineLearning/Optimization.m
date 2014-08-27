Package["MachineLearning`"]

PackageImport["Developer`"]


PackageScope["NewtonDescent"]

NewtonDescent::usage = "NewtonDescent[data, cost, gradient, hessian, theta0] performs simple Newton-method minimisation for a convex cost-function (the hessian has to be definite-positive).
cost, gradient, and hessian are pure functions where the first parameter is data, and the second is the parameters."


PackageScope["GradientDescent"]

GradientDescent::usage = "GradientDescent[data, cost, gradient, theta0, alpha] performs simple Gradient-descent minimisation.
cost and gradient are pure functions where the first parameter is data, and the second is the parameters.
theta0 is the initial parameters vector, and alpha the learning rate.
With \"BatchNumber\"-> n, performs mini-batch gradient descent with n mini-batches.
\"Momentum\"-> m, add a momentum to the descent: for a constant gradient, the effective learning rate will eventually reach m*alpha.
MaxIterations defines the maximum number of iterations.
\"MinimalGradientNorm\" -> eps stops descent when ||gradient||<eps.
\"MonitorStep\" -> p, computes and plot the evolution of the cost each p iterations.
If minibatches are used, data should be of the form {X, Y} or {X} where X is a list of feature vectors (design matrix), and Y a list of responses or list of response vectors.
"

PackageScope["LBFGSDescent"]

LBFGSDescent::usage = "LBFGS optimization function"

Options[NewtonDescent] = {
	AccuracyGoal -> $MachinePrecision,
	PrecisionGoal -> $MachinePrecision,
	MaxIterations -> 10
};

NewtonDescent[data_, costfunc_Function, gradient_Function, hessian_Function, theta0_, opts:OptionsPattern[]] := Module[
	{theta = theta0, thetanew, cost, costnew, grad, hess, deltatheta, deltacost, p, a},
	p = OptionValue[PrecisionGoal];
	a = OptionValue[AccuracyGoal];
	cost = costfunc[data, theta];
	Do[
		grad = gradient[data, theta];
		hess = hessian[data, theta];
		If[!SymmetricMatrixQ[hess], hess = (hess + Transpose[hess])/2.];
		If[PositiveDefiniteMatrixQ[hess],
			deltatheta = -LinearSolve[hess, grad, Method -> "Cholesky"];
			,
			deltatheta = -PseudoInverse[hess].grad; (* may be find something better (ad hoc regularization)*)
		];
		deltacost = (-deltatheta.grad)/2.;
		If[deltacost <= Abs[cost]*10.^-p+10.^-a, Break[]];
		Do[
			thetanew = theta + deltatheta;
			costnew = costfunc[data, thetanew];
			If[costnew <= cost + 0.01*grad.deltatheta, Break[]];
			deltatheta = deltatheta*0.5;
			,
			{100}
		];
		theta = thetanew;
		cost = costnew;
		,
		{OptionValue[MaxIterations]}
	];
	theta
];

(* Very simple gradient descent. 
For more efficient/automatised optimisation, use Conjugate Gradient, Newton, BFGS, or L-BFGS. *)

Options[GradientDescent] = {
	"Momentum" -> 1.,
	"BatchNumber" -> 1,
	MaxIterations -> 100,
	"MinimalGradientNorm" -> 0.,
	"MonitorStep" -> None
};

GradientDescent[data_, costfunc_Function, gradient_Function, theta0_, alpha_, opts:OptionsPattern[]] := Module[
	{cost, grad, mingradnorm, momentum, theta, thetanew, deltatheta, 
		deltathetanew, monitor, monitorstep, costhistory, nbatch, batchspan, batchindex},
	theta = theta0;
	mingradnorm = OptionValue["MinimalGradientNorm"];
	momentum = OptionValue["Momentum"];
	deltatheta = 0.;
	monitorstep = OptionValue["MonitorStep"];
	If[SameQ[monitorstep, None], monitor = False, monitor = True];
	cost = costfunc[data, theta];
	Print["Initial cost = ", cost];
	If[monitor, costhistory = {cost}];
	nbatch = OptionValue["BatchNumber"];
	batchspan = Span[#1+1, #2] & @@@ Partition[sliceBoundaries[Length[First@data], nbatch], 2, 1];
	Do[
		batchindex = Mod[iter-1, nbatch]+1;
		If[nbatch == 1, 
			grad = gradient[data, theta],
			grad = gradient[#[[batchspan[[batchindex]]]] & /@ data, theta];
		];
		If[Sqrt[Total[grad^2, Infinity]] < mingradnorm, Print[iter, " iterations"]; Break[]];
		deltathetanew = -alpha*grad + deltatheta*(1.-1./momentum);
		thetanew = theta + deltathetanew;
		deltatheta = deltathetanew;
		theta = thetanew;
		If[monitor && Mod[iter, monitorstep] == 0,
			cost = costfunc[data, theta];
			AppendTo[costhistory, cost];
		];
		,
		{iter, OptionValue[MaxIterations]}
	];
	cost = costfunc[data, theta];
	Print["Final cost = ", cost];
	If[monitor, 
		costhistory = Transpose[{monitorstep*Range[0, Length[costhistory] - 1], costhistory}];
		Print[
			If[Length[costhistory] > 100,
				ListLogLinearPlot[Rest @ costhistory, Joined -> True]
				,
				ListPlot[costhistory, Joined -> True, PlotMarkers -> Automatic]
			]
		];
	];
	theta
];

(**BFGS stuffs **)

Options[LBFGSDescent]={
	AccuracyGoal->$MachinePrecision,
	PrecisionGoal->$MachinePrecision,
	MaxIterations->100,
	"StoredGradientNumber" -> 20
};

LBFGSDescent[data_, costfunc_Function, gradient_Function, theta0_, opts:OptionsPattern[]]:=Module[
	{memory, theta, grad1, grad2, pk, alpha, pg, ag},
	pg = OptionValue[PrecisionGoal];
	ag = OptionValue[AccuracyGoal];
	memory = {};
	theta = theta0;
	grad1 = gradient[data, theta];
	(* start while *)
	Do[
		pk = -lbfgsdirection[grad1, memory];
		If[pk.grad1>0,
			memory={};
			pk = -lbfgsdirection[grad1, memory]
		];
		alpha = cubicLineSearch[data, theta, pk, grad1, costfunc];
		theta+=alpha*pk;
		grad2 = gradient[data, theta];
		If[Norm[alpha*pk]<=Max[10^(-ag), Norm[theta]*10^(-pg)]&&Norm[grad2]<= 10^(-ag), Break[]];
		memory = If[Length[memory] == OptionValue["StoredGradientNumber"],
			Append[Drop[memory,1],{alpha*pk,grad2-grad1}],
			Append[memory,{alpha*pk,grad2-grad1}]
		];
		If[(grad2-grad1).(grad2-grad1)==0,Break[]];
		grad1=grad2
		,
		{OptionValue[MaxIterations]}
	];
	theta
]

lbfgsdirection[gradient_, memory_]:=Module[
	{q, m, alpha, beta, r, H0, lenq},
	q = gradient; (* put gradient into single list *)
	lenq = Length[q];
	m = Length[memory];
	alpha = ConstantArray[0, m];
	(* Choose initial H *)
	If[m==0,H0=1, (* important to work with sparsematrices *)
		H0=memory[[-1,1]].memory[[-1,2]]/(memory[[-1,2]].memory[[-1,2]])
	];
	(* Do loops *)
	Do[
		alpha[[i]]=memory[[i,1]].q/(memory[[i,2]].memory[[i,1]]);
		q-=alpha[[i]]*memory[[i,2]]
		,
		{i,m,1,-1}
	];
	r=H0*q;
	Do[
		beta=memory[[i,2]].r/(memory[[i,2]].memory[[i,1]]);
		r+=memory[[i,1]]*(alpha[[i]]-beta),
		{i,1,m}
	];
	r
]

Options[cubicLineSearch] = {
	MaxIterations -> 15,
	"MinimumSlope" -> 0.0001,
	"MinimumStepSize" -> 0.(* the minimum step length. Use 0 for now.*)
};

cubicLineSearch[data_, theta_, direction_, gradient0_, costfunc_Function, opts:OptionsPattern[]] := Module[
	{n, m, phi0, phialpha0, phialpha1, phialpha2, alpha0, alpha1, alpha2, phiGrad0, minslope},
	{m, n} = Dimensions[First[data]];
	minslope = OptionValue["MinimumSlope"];
	alpha0 = 1;
	phi0 = costfunc[data, theta]; (* \[Phi](0) *)
	phialpha0 = costfunc[data, theta+alpha0*direction]; (* \[Phi](Subscript[\[Alpha], 0]) *)
	phiGrad0 = gradient0.direction; (* \[Phi]'(0) *)
	If[phialpha0 <= phi0 + minslope*alpha0*phiGrad0, Return[alpha0]]; (*Newton step*)
	(* Assume that step size too big. Compute quadratic interpolant and minimize *)
	alpha1 = -.5*(phiGrad0*alpha0^2)/(phialpha0-phi0-phiGrad0*alpha0);
	phialpha1 = costfunc[data, theta+alpha1*direction];
	If[phialpha1<= phi0+ minslope*alpha1*phiGrad0, Return[alpha1]];
	Do[
		alpha2 = cubicInt[phi0, phialpha0, phialpha1, alpha0, alpha1, phiGrad0];
		If[alpha2 < OptionValue["MinimumStepSize"], Return[alpha1]];
		phialpha2 = costfunc[data, theta+alpha2*direction];
		If[phialpha2<= phi0+ minslope*alpha2*phiGrad0, Return[alpha2]];
		(* Failed to pass Wolfe condition. Following Nocedal, we check that \[Alpha]1 and \[Alpha]2 are not too close together or far apart. 
		If they are, we rescale by factor of two. What counts as 'far apart not given in Nocedal, so we take the Scipy values. This condition also prevents the step size going to 0.  *)
		If[(1 - alpha2/alpha1)<0.96 ||(alpha1 - alpha2) > alpha1/2, alpha2 = alpha1/2];
		(* update values *)
		alpha0 = alpha1;
		alpha1 = alpha2;
		phialpha0 = phialpha1;
		phialpha1 = phialpha2;
		,
		{OptionValue[MaxIterations]}
	];
	(* If this terminates and has failed to satisfy Wolfe condition, we will simply return \[Alpha]2. *)
	alpha2
]

cubicInt[phi0_, phialpha0_, phialpha1_, alpha0_, alpha1_, phiGrad0_] := Module[
	{a, b},
	{a, b} = {{alpha0^2,-alpha1^2},{-alpha0^3,alpha1^3}}.{phialpha1-phi0-phiGrad0*alpha1,phialpha0-phi0-phiGrad0*alpha0}/(alpha0^2 * alpha1^2*(alpha1-alpha0));
	If[a == 0,Return[-phiGrad0/(2*b)]]; (* if a=0, then quadratic. See pg 53 of this. The Scipy implementation does not seem to account for this. *)
	(-b+Sqrt[b^2-3*a*phiGrad0])/(3*a) (* a not 0, hence return cubic *)
]
