



$awsSource = @"
using System;
using System.Globalization;
using System.Text;
using System.Security.Cryptography;

namespace Amazon.DevTools
{
    public class AWSUser
    {
        public string AccessKey
        {
            get;
            set;
        }

        public string SecretKey
        {
            get;
            set;
        }

        protected internal void Validate()
        {
            if (string.IsNullOrEmpty(this.AccessKey))
            {
                throw new InvalidOperationException("[AccessKey]");
            }
            if (string.IsNullOrEmpty(this.SecretKey))
            {
                throw new InvalidOperationException("[SecretKey]");
            }
        }
    }
}

namespace Amazon.DevTools
{
    public abstract class AWSDevToolsRequest
    {
        protected const string METHOD = "GIT";
        protected const string SERVICE = "devtools";

        DateTime dateTime;

        public AWSDevToolsRequest()
            : this(DateTime.UtcNow)
        {
        }

        public AWSDevToolsRequest(DateTime dateTime)
        {
            if (dateTime == null)
            {
                throw new ArgumentNullException("dateTime");
            }
            this.dateTime = dateTime.ToUniversalTime();
        }

        public string DateStamp
        {
            get
            {
                return this.dateTime.ToString("yyyyMMdd", CultureInfo.InvariantCulture);
            }
        }

        public string DateTimeStamp
        {
            get
            {
                return this.dateTime.ToString("yyyyMMddTHHmmss", CultureInfo.InvariantCulture);
            }
        }

        public abstract string DerivePath();

        protected internal abstract string DeriveRequest();

        public string Host
        {
            get;
            set;
        }

        public string Region
        {
            get;
            set;
        }

        public string Service
        {
            get
            {
                return AWSDevToolsRequest.SERVICE;
            }
        }

        protected internal virtual void Validate()
        {
            if (string.IsNullOrEmpty(this.Host))
            {
                throw new InvalidOperationException("[Host]");
            }
            if (string.IsNullOrEmpty(this.Region))
            {
                throw new InvalidOperationException("[Region]");
            }
        }
    }
}

namespace Amazon.DevTools
{
    public class AWSElasticBeanstalkRequest : AWSDevToolsRequest
    {
        public AWSElasticBeanstalkRequest()
            : base()
        {
        }

        public AWSElasticBeanstalkRequest(DateTime dateTime)
            : base(dateTime)
        {
        }

        public string Application
        {
            get;
            set;
        }

        public override string DerivePath()
        {
            this.Validate();

            string path = null;
         
            if (string.IsNullOrEmpty(this.Environment))
            {
                path = string.Format("/v1/repos/{0}/commitid/{1}"
		, this.Encode(this.Application)
		, this.Encode(this.CommitId));
            }
            else
            {
                path = string.Format("/v1/repos/{0}/commitid/{1}/environment/{2}"
		, this.Encode(this.Application)
		, this.Encode(this.CommitId)
		, this.Encode(this.Environment));
            }
            return path;
        }

        protected internal override string DeriveRequest()
        {
            this.Validate();

            string path = this.DerivePath();
            string request = string.Format("{0}\n{1}\n\nhost:{2}\n\nhost\n", AWSDevToolsRequest.METHOD, path, this.Host);
            return request;
        }

        public string Environment
        {
            get;
            set;
        }

	public string CommitId
	{
	   get;
	   set;
	}

        protected internal override void Validate()
        {
            base.Validate();
            if (string.IsNullOrEmpty(this.Application))
            {
                throw new InvalidOperationException("[Application]");
            }
            if (string.IsNullOrEmpty(this.Host))
            {
                throw new InvalidOperationException("[Host]");
            }
        }

	protected internal string Encode(string plaintext)
	{
	    StringBuilder sb = new StringBuilder();
	    foreach (byte b in new UTF8Encoding().GetBytes(plaintext))
	    {
		sb.Append(b.ToString("x2", CultureInfo.InvariantCulture));
	    }
	    return sb.ToString();
	}

    }
}

namespace Amazon.DevTools
{
    public class AWSDevToolsAuth
    {
        const string AWS_ALGORITHM = "HMAC-SHA256";
        const string HASH_ALGORITHM = "SHA-256";
        const string HMAC_ALGORITHM = "HMACSHA256";
        const string SCHEME = "AWS4";
        const string TERMINATOR = "aws4_request";

        AWSUser user;
        AWSDevToolsRequest request;

        public AWSDevToolsAuth(AWSUser user, AWSDevToolsRequest request)
        {
            this.user = user;
            this.request = request;
        }

        static byte[] DeriveKey(AWSUser user, AWSDevToolsRequest request)
        {
            string secret = string.Format("{0}{1}", AWSDevToolsAuth.SCHEME, user.SecretKey);
            byte[] kSecret = Encoding.UTF8.GetBytes(secret);
            byte[] kDate = AWSDevToolsAuth.Hash(AWSDevToolsAuth.HMAC_ALGORITHM, kSecret, Encoding.UTF8.GetBytes(request.DateStamp));
            byte[] kRegion = AWSDevToolsAuth.Hash(AWSDevToolsAuth.HMAC_ALGORITHM, kDate, Encoding.UTF8.GetBytes(request.Region));
            byte[] kService = AWSDevToolsAuth.Hash(AWSDevToolsAuth.HMAC_ALGORITHM, kRegion, Encoding.UTF8.GetBytes(request.Service));
            byte[] key = AWSDevToolsAuth.Hash(AWSDevToolsAuth.HMAC_ALGORITHM, kService, Encoding.UTF8.GetBytes(AWSDevToolsAuth.TERMINATOR));
            return key;
        }

        public string DerivePassword()
        {
            this.user.Validate();
            this.request.Validate();

            string signature = AWSDevToolsAuth.SignRequest(this.user, this.request);
            string password = string.Format("{0}Z{1}", this.request.DateTimeStamp, signature);
            return password;
        }

        public Uri DeriveRemote()
        {
            this.request.Validate();

            string path = this.request.DerivePath();
            string password = this.DerivePassword();
            string username = this.DeriveUserName();
            UriBuilder remote = new UriBuilder()
            {
                Host = this.request.Host,
                Path = path,
                Password = password,
                Scheme = "https",
                UserName = username,
            };
            return remote.Uri;
        }

        public string DeriveUserName()
        {
            this.user.Validate();

            return this.user.AccessKey;
        }

        static byte[] Hash(string algorithm, byte[] message)
        {
            HashAlgorithm hash = HashAlgorithm.Create(algorithm);
            byte[] digest = hash.ComputeHash(message);
            return digest;
        }

        static byte[] Hash(string algorithm, byte[] key, byte[] message)
        {
            KeyedHashAlgorithm hash = KeyedHashAlgorithm.Create(algorithm);
            hash.Key = key;
            byte[] digest = hash.ComputeHash(message);
            return digest;
        }

        static string SignRequest(AWSUser user, AWSDevToolsRequest request)
        {
            string scope = string.Format("{0}/{1}/{2}/{3}", request.DateStamp, request.Region, request.Service, AWSDevToolsAuth.TERMINATOR);
            StringBuilder stringToSign = new StringBuilder();
            stringToSign.AppendFormat("{0}-{1}\n{2}\n{3}\n", AWSDevToolsAuth.SCHEME, AWSDevToolsAuth.AWS_ALGORITHM, request.DateTimeStamp, scope);
            byte[] requestBytes = Encoding.UTF8.GetBytes(request.DeriveRequest());
            byte[] requestDigest = AWSDevToolsAuth.Hash(AWSDevToolsAuth.HASH_ALGORITHM, requestBytes);
            stringToSign.Append(AWSDevToolsAuth.ToHex(requestDigest));
            byte[] key = AWSDevToolsAuth.DeriveKey(user, request);
            byte[] digest = AWSDevToolsAuth.Hash(AWSDevToolsAuth.HMAC_ALGORITHM, key, Encoding.UTF8.GetBytes(stringToSign.ToString()));
            string signature = AWSDevToolsAuth.ToHex(digest);
            return signature;
        }

        static string ToHex(byte[] data)
        {
            StringBuilder hex = new StringBuilder();
            foreach (byte b in data)
            {
                hex.Append(b.ToString("x2", CultureInfo.InvariantCulture));
            }
            return hex.ToString();
        }
    }
}

"@

Add-Type -Language CSharpVersion3 -TypeDefinition $awsSource

# -*-powershell-*-

#
# Sets the AWS.push configuration values
#
# Will read values from the pipeline input if none are present the values are read from the console input instead.
#
function Edit-AWSElasticBeanstalkRemote
{
    $data=@($input)
    $used=0

    $config = Read-Config $False $True
    $awsAccessKey = Lookup-Setting $config "global" "AWSAccessKeyId" ("cred","git")
    if (!$awsAccessKey -and (ShouldWrite-Credentials $config $false))
    {
        $awsAccessKeyInput = ($data[$used++] | Input-Data "AWS Access Key")
    }
    if ($awsAccessKeyInput)
    {
        $config = Write-Setting $config "cred" "global" "AWSAccessKeyId" $awsAccessKeyInput
    }

    $awsSecretKey = Lookup-Setting $config "global" "AWSSecretKey" ("cred","git")
    if (!$awsSecretKey -and (ShouldWrite-Credentials $config $false))
    {
        $awsSecretKeyInput = ($data[$used++] | Input-Data "AWS Secret Key")
    }
    if ($awsSecretKeyInput)
    {
        $config = Write-Setting $config "cred" "global" "AWSSecretKey" $awsSecretKeyInput
    }

    $awsRegion = Lookup-Setting $config "global" "Region" ("eb","git")
    if (-not $awsRegion)
    {
        $awsRegion = "us-east-1"
        $config = Write-Setting $config "eb" "global" "Region" $awsRegion
    }
    $awsRegionInput = ($data[$used++] | Input-Data "AWS Region [default to $($awsRegion)]")
    if ($awsRegionInput)
    {
    $awsRegion = $awsRegionInput
        $config = Write-Setting $config "eb" "global" "Region" $awsRegionInput
    }

    $awsHost = Get-Endpoint $awsRegion
    if ($awsHost)
    {
        $config = Write-Setting $config "eb" "global" "DevToolsEndpoint" $awsHost
    }
    else
    {
        $awsHostInput = ($data[$used++] | Input-Data "AWS Host [default to git.elasticbeanstalk.us-east-1.amazonaws.com]")

        if ($awsHostInput)
        {
            $config = Write-Setting $config "eb" "global" "DevToolsEndpoint" $awsHostInput
        }
        else
        {
            $config = Write-Setting $config "eb" "global" "DevToolsEndpoint" "git.elasticbeanstalk.us-east-1.amazonaws.com"
        }
    }

    $awsApplication = Lookup-Setting $config "global" "ApplicationName" ("eb","git")
    if ($awsApplication)
    {
        $awsApplicationInput = ($data[$used++] | Input-Data "AWS Elastic Beanstalk Application [default to $($awsApplication)]")
    }
    else
    {
        $awsApplicationInput = ($data[$used++] | Input-Data "AWS Elastic Beanstalk Application")
    }
    if ($awsApplicationInput)
    {
        $config = Write-Setting $config "eb" "global" "ApplicationName" $awsApplicationInput
    }

    $awsEnvironment = Lookup-Setting $config "global" "EnvironmentName" ("eb","git")
    if ($awsEnvironment)
    {
        $awsEnvironmentInput = ($data[$used++] | Input-Data "AWS Elastic Beanstalk Environment [default to $($awsEnvironment)]")
    }
    else
    {
        $awsEnvironmentInput = ($data[$used++] | Input-Data "AWS Elastic Beanstalk Environment")
    }
    if ($awsEnvironmentInput)
    {
        $config = Write-Setting $config "eb" "global" "EnvironmentName" $awsEnvironmentInput
    }

    Write-Config $config
}

#
# Looks up the endpoint for a region
#
# @params $region
# @return The endpoint
#
function Get-Endpoint
{
    Param($region)
    switch ($region)
    {
        "us-east-1" { $endpoint = "git.elasticbeanstalk.us-east-1.amazonaws.com" }
        "ap-northeast-1" { $endpoint = "git.elasticbeanstalk.ap-northeast-1.amazonaws.com" }
        "ap-southeast-1" { $endpoint = "git.elasticbeanstalk.ap-southeast-1.amazonaws.com" }
        "ap-southeast-2" { $endpoint = "git.elasticbeanstalk.ap-southeast-2.amazonaws.com" }
        "eu-west-1" { $endpoint = "git.elasticbeanstalk.eu-west-1.amazonaws.com" }
        "sa-east-1" { $endpoint = "git.elasticbeanstalk.sa-east-1.amazonaws.com" }
        "us-west-1" { $endpoint = "git.elasticbeanstalk.us-west-1.amazonaws.com" }
        "us-west-2" { $endpoint = "git.elasticbeanstalk.us-west-2.amazonaws.com" }
    }
    $endpoint
}
#
# Gets the remote URL used for AWS.push
#
# @param $e environment that is deployed too. 
# @param $c commit to deploy
# @return The URL
#
function Get-AWSElasticBeanstalkRemote
{
    Param([string] $e,
          [string] $c,
          [bool] $toPush = $FALSE )
    trap [System.Management.Automation.MethodInvocationException]
    {
        if ($_.Exception -and $_.Exception.InnerException)
        {
            $awsOption = $_.Exception.InnerException.Message
            switch ($awsOption)
            {
                "[AccessKey]" { $awsOption = "aws.accesskey" }
                "[Application]" { $awsOption = "aws.elasticbeanstalk.application" }
                "[Host]" { $awsOption = "aws.elasticbeanstalk.host" }
                "[Region]" { $awsOption = "aws.region" }
                "[SecretKey]" { $awsOption = "aws.secretkey" }
            }
            Write-Host "Missing configuration setting for: $($awsOption)"
        }
        else
        {
            Write-Host "An unknown error occurred while computing your temporary password."
        }
        Write-Host "`nTry running 'git aws.config' to update your repository configuration."
        Exit
    }

    $config = Read-Config
    $awsAccessKey = Lookup-Setting $config "global" "AWSAccessKeyId" ("cred","git")
    $awsSecretKey = Lookup-Setting $config "global" "AWSSecretKey" ("cred","git")
    $awsRegion = Lookup-Setting $config "global" "Region" ("eb","git")
    $awsHost = Lookup-Setting $config "global" "DevToolsEndpoint" ("eb","git")
    $awsApplication = Lookup-Setting $config "global" "ApplicationName" ("eb","git")
 
    if ($e)
    {  
      $awsEnvironment = $e
    }
    else
    {
      $branchName = &git rev-parse --abbrev-ref HEAD
      $defaultEnv = Lookup-Setting $config "branches" $branchName ("eb")
      if ($defaultEnv)
      {
        $awsEnvironment = $defaultEnv 
      }
      else
      {
        $awsEnvironment = Lookup-Setting $config "global" "EnvironmentName" ("eb","git")  
      }
    }

    $gitCommitId = $c

    $awsUser = New-Object -TypeName Amazon.DevTools.AWSUser
    $awsUser.AccessKey = $awsAccessKey
    $awsUser.SecretKey = $awsSecretKey

    $awsRequest = New-Object -TypeName Amazon.DevTools.AWSElasticBeanstalkRequest
    $awsRequest.Region = $awsRegion
    $awsRequest.Host = $awsHost
    $awsRequest.Application = $awsApplication
    $awsRequest.Environment = $awsEnvironment
    $awsRequest.CommitId = $gitCommitId

    $awsAuth = New-Object -TypeName Amazon.DevTools.AWSDevToolsAuth $awsUser,$awsRequest
    $awsRemote = $awsAuth.DeriveRemote()
    if($toPush) {
       Write-Host "Pushing to environment: $awsEnvironment"  
    }  
    return $awsRemote.ToString()
}

#
# Performs the aws.push
#
# @param $e environment that is deployed too. 
# @param $c commit to deploy
#
function Invoke-AWSElasticBeanstalkPush
{
    Param([string] $e, [string] $c)
    $remote = Get-AWSElasticBeanstalkRemote $e $c $TRUE
    $src = $c
    $dst = "refs/heads/master"
    $commit = $src + ":" + $dst
    &git push -f $remote $commit
}

#
# Adds the git aliases for aws.push and aws.config to the git repository.
#
function Initialize-AWSElasticBeanstalkRepository
{
    $command = 'Import-Module AWSDevTools; $e, $c = Get-Options $args; Get-AWSElasticBeanstalkRemote $e $c'
    &git config alias.aws.elasticbeanstalk.remote "!powershell -noprofile -executionpolicy bypass -command '& { $command }'"

    $command = 'Import-Module AWSDevTools; $e, $c = Get-Options $args; Invoke-AWSElasticBeanstalkPush $e $c'
    &git config alias.aws.push "!powershell -noprofile -executionpolicy bypass -command '& { $command }'"

    $command = 'Import-Module AWSDevTools; Edit-AWSElasticBeanstalkRemote'
    &git config alias.aws.config "!powershell -noprofile -executionpolicy bypass -command '& { $command }'"
}

#
# Read in data
#
# Will used pipeline data if present, otherwise reads from the console
#
# @param $message The text to display as a prompt
# @return The data collected
#
function Input-Data
{
    Param([string] $message)
    Write-Host -NoNewline "$($message): "
    if (($input.MoveNext()) -and ($input.Current))
    {
        Write-Host $input.Current
        $input.Current
    }
    else
    {
        [Console]::In.ReadLine() 
    }
}

#
# Gets the values for the aws.push and aws.config command options
#
# @param $arr The command line options passed to the command
# @return The options values
#
function Get-Options
{
    Param([string[]] $arr)

    $e = $null;
    $c = $null;

    $optionmappings = @{
        '--environment' = 'environment';
        '-e' = 'environment';
        '--commit' = 'commit';
        '-c' = 'commit';
        '--help' = 'help';
        '-h' = 'help'
    }

    $options = @{}
    
    for ($i=0; $i -lt $arr.count; $i++)
    {
        $optname = $arr[$i]
        $mappedoption = $optionmappings[$optname]
        if (!$mappedoption) {
            Write-Host("Unknown Option: {0}" -f $arr[$i])
            Write-Help
            Exit
        }
        if ($mappedoption -eq "help") {
            Write-Help
            Exit
        }

        $value = $arr[++$i]
        if (($value -eq $null) -or $optionmappings[$value]) {
            Write-Host("You must provide a value for {0}" -f $optname)
            Write-Help
            Exit
        }
        if ($options[$mappedoption]) {
            Write-Host("--{0} specified twice" -f $mappedoption)
            Exit
        }
        $options[$mappedoption] = $value
    }

    $e = $options["environment"]
    $c = $options["commit"]

    if ($c -eq $null) {
        $c = "HEAD"
    }
    $c = Parse-CommitOption $c
    $result = $e, $c
    $result
}

#
# Looks up a commit to get the id and verifies it is a commit object.
#
# @param $c The commit to get the id of.
# @return The commit id
#
function Parse-CommitOption
{
    Param([string] $c)
    $parsed = &"git" "rev-parse" $c
    if ($LastExitCode -ne 0) {
        Exit
    }
    $type = &"git" "cat-file" "-t" $parsed
    if ($type -ne "commit") {
        Write-Host("{0} is a {1}, and the value of --commit must refer to a commit" -f
                   $c, $type)
        Write-Help
        Exit
    }
    $parsed
}

#
# Display the usage of aws.push
#
function Write-Help
{
    Write-Host @'
Usage: git aws.push

Options:
   --environment ENVIRONMENT, -e ENVIRONMENT
       ENVIRONMENT is the name of an AWS Elastic Beanstalk environment.  When this
       option is used, the command updates the named environment instead of
       the default one.  The default can be set by running "git aws.config".

   --commit COMMIT, -c COMMIT
       COMMIT identifies a commit in the repository -- for example, "HEAD" identifies
       the commit that is currently checked out, or a SHA1 (possibly abbreviated) can
       be used to identify a specific commit from the history.  When this option is used,
       the command uses the named commit instead of HEAD to create the version to be
       deployed to your environment.  See the help for "git rev-parse" for a description
       of all the supported formats for identifying commits.

'@
}

#
# Reads in the config values
#
# @param $verboseOutput = display the config values found.
# @param $credentialOutput = display the credential file location and values found.
# @return The config read
#
function Read-Config($verboseOutput = $FALSE, $credentialOutput = $FALSE)
{
    $gitSettings = @{}
    $gitSettings["global"] = @{}
    $gitSettings["global"]["AWSAccessKeyId"] = &git config --get aws.accesskey
    $gitSettings["global"]["AWSSecretKey"] = &git config --get aws.secretkey
    $gitSettings["global"]["Region"] = &git config --get aws.region
    $gitSettings["global"]["DevToolsEndpoint"] = &git config --get aws.elasticbeanstalk.host
    $gitSettings["global"]["ApplicationName"] = &git config --get aws.elasticbeanstalk.application
    $gitSettings["global"]["EnvironmentName"] = &git config --get aws.elasticbeanstalk.environment
    if ($verboseOutput)
    {
        foreach ($key in $gitSettings["global"].keys)
        {
            if ($gitSettings["global"][$key])
            {
                Write-Host "Read setting in git.config for $($key) of $($gitSettings["global"][$key])"
            }
        }
    }

    $ebConfigFile = Get-Location
    $ebConfigFile = Join-Path $ebConfigFile ".elasticbeanstalk"
    $ebConfigFile = Join-Path $ebConfigFile "config"
    $ebSettings = @{}
    if (Test-Path $ebConfigFile)
    {
        $ebSettings = Read-IniFile $ebConfigFile
    }
    if ((($ebSettings["global"]) -and ($ebSettings["global"]["Region"])) -and (-not($ebSettings["global"]["DevToolsEndpoint"])))
    {
        $endpoint = Get-Endpoint $ebSettings["global"]["Region"]
        if ($endpoint)
        {
            $ebSettings["global"]["DevToolsEndpoint"] = $endpoint
        }
    }
    if ($verboseOutput)
    {
        foreach ($section in $ebSettings.keys)
        {
            foreach ($key in $ebSettings[$section].keys)
            {
                Write-Host "Read setting in $($ebConfigFile) in $($section) for $($key) of $($ebSettings[$section][$key])"
            }
        }
    }

    if (($ebsettings) -and ($ebsettings["global"]))
    {
        $credentialFile = $ebsettings["global"]["AwsCredentialFile"]
    }
    if ((-not($credentialFile -and (Test-Path $credentialFile ))) -and (Test-Path env:USERPROFILE))
    {
        $credentialFile = (Get-Item env:USERPROFILE).value
        $credentialFile = Join-Path $credentialFile ".elasticbeanstalk"
        $credentialFile = Join-Path $credentialFile "aws_credential_file"
    }
    if (Test-Path env:AWS_CREDENTIAL_FILE)
    {
        $envVar = Get-Item env:AWS_CREDENTIAL_FILE
        $credentialFile = $envVar.Value
    }

    $credentials = @{}
    if (($credentialFile) -and (Test-Path $credentialFile))
    {
        try
        {
            $credentials = Read-IniFile $credentialFile
        }
        catch {}
    }
    if ($verboseOutput)
    {
        foreach ($section in $credentials.keys)
        {
            foreach ($key in $credentials[$section].keys)
            {
                Write-Host "Read setting in $($credentialFile) in $($section) for $($key) of $($credentials[$section][$key])"
            }
        }
    }
    if ($credentialOutput)
    {
        if ($credentialFile)
        {
            Write-Host @"
Reading Credentials from $($credentialFile).

"@
            if (Test-Path $credentialFile)
            {
                try
                {
                    $temp = Get-Item $credentialFile -Force
                    if ($credentials["global"] -and ($credentials["global"]["AWSAccessKeyId"] -and $credentials["global"]["AWSSecretKey"]))
                    {
                        Write-Host @"
You can supply different credentials by editing that file or editing .elasticbeanstalk/config to reference a different file.
"@
                    }
                    else
                    {
                        Write-Host @"
This file doesn't contain a full set of credentials.  You can fix the problem by editing the file or by editing .elasticbeanstalk/config to reference a different file.	
"@
                    }
                }
                catch
                {
                    Write-Host @"
The file is not readable.  You can fix the problem by granting read permissions to the file or by editing .elasticbeanstalk/config to reference a different file.
"@
                }
            }
            else
            {
                Write-Host @"
The file does not exist.  You can supply credentials by creating the file or editing .elasticbeanstalk/config to reference a different file.  
"@
            }	
        }
        else
        {
            Write-Host @"
No credential file location found.
You can specify the credential file location by editting .elasticbeanstalk/config and adding the following line:

AwsCredentialFile=<path_to_your_credential_file>

"@
        }
        Write-Host @"
The credential file should have the following format:

AWSAccessKeyId=your key
AWSSecretKey=your secret

"@
    }

    @{git=$gitSettings;eb=$ebSettings;cred=$credentials}
}

#
# Reads an ini file into a hashmap
#
# @param $path The location of the file to read
# @return The ini read
#
function Read-IniFile
{
    Param([string] $path)
    $ini = @{}
    $section = "global"
    $ini[$section] = @{}

    switch -regex -file $path
    {
        "^\[(.+)\]$" # Section
        {
            $section = $matches[1]
            if (!$ini[$section]) 
            {
                $ini[$section] = @{}
            }
            continue
        }
        "^(;.*)" # Comment
        {
            continue
        } 
        "\s*(.+?)\s*=\s*(.*?)\s*$" # Key
        {
            $name,$value = $matches[1..2]
            $ini[$section][$name] = $value
            continue
        }
    }

    return $ini
}

#
# Looks up a value from the config
#
# @param $config The config to look in
# @param $section The section of the value being looked for.
# @param $key The key for the value being looked for.
# @param $locations An in order array of parts of the config to look in.
# @return The value looked up
#
function Lookup-Setting($config, $section, $key, $locations)
{
    foreach ($loc in $locations)
    {
        if (((($config) -and ($config[$loc])) -and ($config[$loc][$section])) -and ($config[$loc][$section][$key]))
        {
            $setting = $config[$loc][$section][$key]
            break;
        } 
    }
    $setting
}

#
# Sets a value in a config
#
# @param $config The config to set the value in
# @param $file The part of the config to set the value in
# @param $section The section the value should be in
# @param $key The key for the value being set
# @param $value The value that is being set
# @return The config with the value set
#
function Write-Setting($config, $file, $section, $key, $value)
{
    if (!$config)
    {
        $config = @{}
    }
    if (!$config[$file])
    {
        $config[$file] = @{}
    }
    if (!$config[$file][$section])
    {
        $config[$file][$section] = @{}
    }
    $config[$file][$section][$key] = $value
    $config
}

#
# Write out the config
#
# @param $config The config to write
#
function Write-Config($config)
{
    foreach ($var in ("Region", "DevToolsEndpoint", "ApplicationName", "EnvironmentName"))
    {
        if ((!(Lookup-Setting $config "global" $var ("eb"))) -and (Lookup-Setting $config "global" $var ("git")))
        {
            $config = Write-Setting $config "eb" "global" $var (Lookup-Setting $config "global" $var ("git"))
        }
    }
    foreach ($var in ("AWSAccessKeyId", "AWSSecretKey"))
    {
        if ((!(Lookup-Setting $config "global" $var ("cred"))) -and (Lookup-Setting $config "global" $var ("git")))
        {
            $config = Write-Setting $config "cred" "global" $var (Lookup-Setting $config "global" $var ("git"))
        }
    }

    $credentialFile = ShouldWrite-Credentials $config
    if ($credentialFile) 
    {
        Write-IniFile $config["cred"] $credentialFile
        $config = Write-Setting $config "eb" "global" "AwsCredentialFile" $credentialFile
    }
    
    if (($config) -and (($config["eb"]) -and (@($config["eb"].keys).length -ne 0)))
    {
        $ebConfigFile = Get-Location
        $ebConfigFile = Join-Path $ebConfigFile ".elasticbeanstalk"
        $ebConfigFile = Join-Path $ebConfigFile "config"
        Write-IniFile $config["eb"] $ebConfigFile
    }
}

#
# ShouldWrite-Credentials
#
# @param $config The config to we will be writing
# @return The location to write to
#
function ShouldWrite-Credentials($config, $checkForCreds = $True)
{
    if ((!$checkForCreds) -or (($config) -and (($config["cred"]) -and (@($config["cred"].keys).length -ne 0))))
    { 
        if (-not((Test-Path env:AWS_CREDENTIAL_FILE) -or (Lookup-Setting $config "global" "AwsCredentialFile" ("eb"))))
        {
            if (Test-Path env:USERPROFILE)
            {
                $credentialFile = (Get-Item env:USERPROFILE).value
                $credentialFile = Join-Path $credentialFile ".elasticbeanstalk"
                $credentialFile = Join-Path $credentialFile "aws_credential_file"
                if (Test-Path $credentialFile) 
                {
                    $credentialFile = $null
                }
            }
        }
    }

    $credentialFile
}


#
# Write an ini file
#
# @param $ini The ini that should be written to the file
# @param $path The location of the ini to write
#
function Write-IniFile($ini, $path)
{
    $iniFile = New-Item -ItemType file -Path $path -Force
    if (($ini["global"]) -and ($ini["global"].keys.count -gt 0))
    {
        Add-Content -Path $iniFile -Value "[global]"
        foreach ($i in $ini["global"].keys | Sort-Object)
        {
            Add-Content -Path $iniFile -Value "$i=$($ini["global"][$i])" 
        }
    }
    foreach ($i in $ini.keys | Sort-Object)
    {
        if (!($i -eq "global"))
        {
            Add-Content -Path $iniFile -Value "[$i]"
            foreach ($j in ($ini[$i].keys | Sort-Object))
            {
                Add-Content -Path $iniFile -Value "$j=$($ini[$i][$j])" 
            }
        }
    }
}

#
# Installs the module in powershell
#
function Install-AWSDevToolsModule
{
    $userPath = $env:PSModulePath.split(";")[0]
    $modulePath = Join-Path $userPath -ChildPath AWSDevTools
    if (-not (Test-Path $modulePath))
    {
        New-Item -Path $modulePath -ItemType directory | Out-Null
    }
    Get-ChildItem AWSDevTools -Recurse | ForEach-Object { Copy-Item $_.fullName -Destination $modulePath -Force }
}

