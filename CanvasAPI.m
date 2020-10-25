(* ::Package:: *)

(* Create new group of questions *)
Options[CreateQuestionGroup]={
	"PrintHTTPResponse" -> True, (* Print the code of HTTP response, to check whether it's OK *)
	"PrintHTTPData" -> False(* Print details of HTTP request/responses *) 
};

CreateQuestionGroup[GroupName_, CourseID_, QuizID_, CanvasURL_, AccessToken_, OptionsPattern[]]:=Module[
	{RequestToServer, ResponseFromServer},
	RequestToServer = HTTPRequest[
		CanvasURL<>"/api/v1/courses/"<>ToString[CourseID]<>"/quizzes/"<>ToString[QuizID]<>"/groups",
		<|Method->"POST", "Headers"->{"Authorization"->"Bearer "<>AccessToken},
		"Body" -> {"quiz_groups[][name]" -> GroupName, "quiz_groups[][pick_count]" -> 1, "quiz_groups[][question_points]" -> 1}|>];
	ResponseFromServer = URLRead[RequestToServer]; 
	If[OptionValue["PrintHTTPResponse"], Print[ResponseFromServer];];
	If[OptionValue["PrintHTTPData"], Print[ResponseFromServer["Body"]];];
	(* We return the id of the newly created group *)
	ImportString[ResponseFromServer["Body"],"RawJSON"]["quiz_groups"][[1]]["id"]
];

(* Upload question to an existing quiz *)
Options[UploadQuizQuestion]={
	"PrintHTTPResponse" -> True, (* Print the code of HTTP response, to check whether it's OK *)
	"PrintHTTPData" -> False (* Print details of HTTP requests/responses *) 
};

UploadQuizQuestion[QSpec_, CourseID_, QuizID_, CanvasURL_, AccessToken_, OptionsPattern[]] := Module[
	{JSONString, RequestToServer, ResponseFromServer, ResponseData},
	JSONString = ExportString[<|"question"->QSpec|>, "RawJSON"];
	If[OptionValue["PrintHTTPData"], Print[JSONString]];
	RequestToServer = HTTPRequest[
		CanvasURL<>"/api/v1/courses/"<>ToString[CourseID]<>"/quizzes/"<>ToString[QuizID]<>"/questions",
		<|Method->"POST", "Headers"->{"Authorization"->"Bearer "<>AccessToken}, "ContentType" -> "application/json", "Body" -> JSONString|>];
	ResponseFromServer = URLRead[RequestToServer]; 
	If[OptionValue["PrintHTTPResponse"], Print["Response from server: ", ResponseFromServer];];
	ResponseData = ImportString[ResponseFromServer["Body"], "RawJSON"];
	If[OptionValue["PrintHTTPData"], Print["Data obtained from the server: ", ResponseData];];
	(* We return question ID *)
	ResponseData["id"]
];

(* Upload an image file to Canvas *)
Options[UploadGraphics] = {
	"PrintHTTPResponse" -> True, (* Print the code of HTTP response, to check whether it's OK *)
	"PrintHTTPData"->False, (* Print details of HTTP request/responses *) 
	"TmpDir"->"", (* Temp. dir to save files *)
	"AutoDeleteFile"->True,
	"UploadedFilePath"->"Uploaded Media"
};

UploadGraphics[Gr_, CourseID_, FileName_, CanvasURL_, AccessToken_, OptionsPattern[]] := Module[
	{GraphicsFileType, FullFileName, FSize, RequestData, RequestToServer, ResponseFromServer, ResponseData, FileURL, ImageHTMLTag},
	GraphicsFileType = ToLowerCase[FileExtension[FileName]];
	FullFileName = OptionValue["TmpDir"]<>FileName;
	Export[FullFileName, Gr];
	FSize = FileByteCount[FullFileName];
	(* According to this document https://canvas.instructure.com/doc/api/file.file_uploads.html, we first tell Canvas about the file upload and get the upload token *)
	RequestData = <|"name" -> FileName, "size" -> FSize, "content_type" -> "image/"<>GraphicsFileType, "parent_folder_path" -> OptionValue["UploadedFilePath"]|>;
	If[OptionValue["PrintHTTPData"], Print["Data to be sent to the server in the first request (getting upload token): ", RequestData]];
	RequestToServer = HTTPRequest[CanvasURL<>"/api/v1/courses/"<>ToString[CourseID]<>"/files", 
		<|Method->"POST", "Headers"->{"Authorization"->"Bearer "<>AccessToken}, "Body"->RequestData|>];
	ResponseFromServer = URLRead[RequestToServer];
	If[OptionValue["PrintHTTPResponse"], Print["Response from the server: ",ResponseFromServer];]; 
	ResponseData = ImportString[ResponseFromServer["Body"],"RawJSON"];
	If[OptionValue["PrintHTTPData"], Print["Data obtained from the server: ", ResponseData];];
	(* Preparing the second HTTP request to actually upload the file, note that now it's to a different URL and we no longer need the AccessToken *)
	RequestData=ResponseData["upload_params"];
	AssociateTo[RequestData, "file"->File[FullFileName]]; (* We use Mathematica's File[] here to let Mathematica take care of the file upload in HTTP request automatically *)
	If[OptionValue["PrintHTTPData"], 
		Print["Data to be sent to server in the second request (actually uploading the file):", RequestData];
		Print["Upload URL: ", ResponseData["upload_url"]];
	];
	RequestToServer = HTTPRequest[ResponseData["upload_url"],
		<|Method->"POST", "ContentType"->"multipart/form-data", "Body"->RequestData, "Expect"->""|>];
	ResponseFromServer = URLRead[RequestToServer];
	If[OptionValue["PrintHTTPResponse"], Print["Response from the server: ",ResponseFromServer];]; 
	ResponseData = ImportString[ResponseFromServer["Body"],"RawJSON"];
	If[OptionValue["PrintHTTPData"], Print["Data obtained from the server: ", ResponseData];];
	FileURL = CanvasURL<>"/courses/"<>ToString[CourseID]<>"/files/"<>ToString[ResponseData["id"]];
	ImageHTMLTag = 
	"<img id=\""<>ToString[ResponseData["id"]]<>"\" src=\""<>FileURL<>"/preview\" alt=\""<>FileName<>"\" data-api-endpoint=\""<>FileURL<>"\" data-api-returntype=\"File\" />";
	If[OptionValue["AutoDeleteFile"], DeleteFile[FullFileName]];
	(* Return the image ID and the HTML tag which can be used to embed the image in Canvas docs *)
	{ResponseData["id"],ImageHTMLTag}
];
