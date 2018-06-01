var fso = new ActiveXObject("Scripting.FileSystemObject");
var shell = WScript.CreateObject("WScript.Shell");
var net = WScript.CreateObject("WScript.Network");

var winver = shell.RegRead("HKLM\\Software\\Microsoft\\Windows NT\\CurrentVersion\\CurrentVersion");
var arch = shell.RegRead("HKLM\\SYSTEM\\CurrentControlSet\\Control\\Session Manager\\Environment\\PROCESSOR_ARCHITECTURE");

var wbemFlagReturnWhenComplete = 0;
var wbemFlagForwardOnly = 0x20;

var windir = shell.ExpandEnvironmentStrings("%SystemRoot%");
var system32 = shell.ExpandEnvironmentStrings("%SystemRoot%\\System32");

// Para que PSKill no moleste con el EULA
shell.Run("reg.exe delete HKCU\\Software\\Sysinternals\\PsKill /f", 0, true);
shell.Run("pskill.exe /accepteula", 0, true);

var ccmsetupdir = windir + "\\ccmsetup";
var ccmdir = windir + "\\CCM";
var ccmcache = windir + "\\ccmcache";

function Log(file, msg) {
    var today = new Date();
    file.WriteLine(today + ": " + msg);
}

function Exit(code, desc) {
    Log(logf, "* Finalizado con código: " + code + " " + desc);
    WScript.Quit(code);
}

function ConfigureRemoteRegistry() {
	Log(logf, "* Configurando registro remoto");
	shell.Run("sc.exe config RemoteRegistry start=auto");
	shell.Run("sc.exe start RemoteRegistry");
}

function EnableRemoting() {
	Log(logf, "* Configurando acceso remoto");
	shell.Run("winrm quickconfig -quiet -force");
}

function FixPolicy() {
    Log(logf, "* Reseteando group policy");
    var policyPath = system32 + "\\GroupPolicy\\Machine";
    var backup = policyPath + "\\registry.old";
    if (fso.FileExists(backup)) {
        fso.DeleteFile(backup);
    }
    try {
        fso.MoveFile(policyPath + "\\registry.pol", backup);
    }
    catch (e) {}
    shell.Run("gpupdate.exe /force", 0, true);
}

function FixMSI () {
    Log(logf, "* Registrando MSI");
    Log(logf, "  + Deteniendo el servicio");
    shell.Run("net stop MSIServer /y", 0, true);
    
    Log(logf, "  + Matando msiexec.exe");
    shell.Run("pskill.exe -t msiexec.exe", 0, true);
    
    Log(logf, "  + Deregistrando");
    shell.Run("msiexec.exe /unreg", 0, true);
    Log(logf, "  + Registrando");
    shell.Run("msiexec.exe /regserver", 0, true);
}

function RegisterLibraries () {
    // Registrar librerías
    Log(logf, "  + Registrando librerías");
    var fwbem = fso.GetFolder(system32 + "\\wbem");
    var en = new Enumerator(fwbem.Files);
    for (; !en.atEnd(); en.moveNext()) {
        var file = en.item();
        if (file.Name.slice(-3).toLowerCase() == "dll") {
            shell.Exec("regsvr32.exe /s " + fwbem.Path + "\\" + file.Name);
        }
    }
}

function CompileMOF () {
    // Compilar MOF
    Log(logf, "  + Compilando archivos MOF");
    var fwbem = fso.GetFolder(system32 + "\\wbem");
    var en = new Enumerator(fwbem.Files);
    for (; !en.atEnd(); en.moveNext()) {
        var file = en.item();
        if ((file.Name.slice(-3).toLowerCase() == "mof") ||
             (file.name.slice(-3).toLowerCase() == "mfl")) {
            Log(logf, "    . compilando " + file.Name);
            shell.Run("mofcomp " + fwbem.Path + "\\" + file.Name, 0, true);
        }
    }
}

function FixWMI () {
    Log(logf, "* Reparando WMI");
    Log(logf, "  + Bajando servicios");
    shell.Run("net stop ccmexec /y", 0, true);
    shell.Run("net stop ccmsetup /y", 0, true);
    shell.Run("pskill.exe -t ccmsetup.exe", 0, true);
    shell.Run("pskill.exe -t ccmrepair.exe", 0, true);
    shell.Run("sc config winmgmt start= disabled", 0, true);
    shell.Run("net stop winmgmt /y", 0, true);
    shell.Run("winmgmt /kill", 0, true);

    // Windows 7
    Log(logf, "  + Recuperando repositorio");
    shell.Run("sc config winmgmt start= auto", 0, true);
    shell.Run("winmgmt /standalonehost", 0, true);
    shell.Run("winmgmt /resetrepository", 0, true);
    RegisterLibraries();
    CompileMOF();

    shell.Run("sc config winmgmt start= auto", 0, true);
    shell.Run("net start winmgmt", 0, true);
    WScript.Sleep(60000);
}

function UninstallClient () {
    Log(logf, "* Desinstalando cliente");
    Log(logf, "  + Deteniendo servicios");
    shell.Run("net stop ccmsetup /y", 0, true);
    shell.Run("net stop ccmexec", 0, true);

    try {
        var wmi = GetObject("winmgmts:\\\\.\\root\\cimv2");
    } catch (e) {
        Log(logf, "  !! No se pudo acceder a WMI");
        Exit(-1, "No se pudo acceder a WMI luego de la reparación");
    }

    Log(logf, "  + Matando ccmsetup.exe");
    do {
        var ret = shell.Run("pskill.exe -t ccmsetup.exe", 0, true);
        WScript.Sleep(1000);
    } while (ret != -1);

    Log(logf, "  + Desinstalando cliente");
    var counter = 1;
    do {
    	// Si el contador llegó a 0 es porque se intentó desinstalar usando el ccmsetup local.
    	// Esta vez se intenta usando el que está en el share.
    	if (counter == 0) {
            shell.Run("\\\\trabajo\\netlogon\\clientesms\\ccmsetup.exe /uninstall", 0, true);    		
    	} else {
	        if (fso.FileExists(system32 + "\\ccmsetup\\ccmsetup.exe")) {
	            shell.Run(system32 + "\\ccmsetup\\ccmsetup.exe /uninstall", 0, true);
	        } else if (fso.FileExists(ccmsetupdir + "\\ccmsetup.exe")) {
	            shell.Run(ccmsetupdir + "\\ccmsetup.exe /uninstall", 0, true);
	        } else {
	            // No existe el instalador del cliente, se intenta desinstalación por instalador en red
	            shell.Run("\\\\trabajo\\netlogon\\clientesms\\ccmsetup.exe /uninstall", 0, true);
	        }    		
    	}

        WScript.Sleep(300000);

        // Chequear si se desinstaló el cliente
        var col = wmi.ExecQuery("select * from Win32_Product where Name = \"Configuration Manager Client\"",
                                "WQL", wbemFlagForwardOnly | wbemFlagReturnWhenComplete);
        
        var en = new Enumerator(col);
    } while (!en.atEnd() || (counter--) != 0);
    
    try {
        fso.DeleteFolder(ccmdir, true);
        fso.DeleteFolder(ccmcache, true);
    } catch (e) {
        Log(logf, "  !! No se pudo eliminar la carpeta CCM o cache");
    }
}

function DeleteCCMNamespace() {
    Log(logf, "* Eliminando espacio de nombres CCM");
    try {
        var wmi = GetObject("winmgmts:\\\\.\\root");
        var item = wmi.Get("__Namespace.Name='ccm'");
        item.Delete();
    } catch (e) {
        Log(logf, "  !! No se pudo eliminar el espacio de nombres CCM");
    }
}

function RemoveCertificates() {
	Log(logf, "* Eliminando certificados");
	if (fso.FileExists(system32 + "\\SMSCFG.ini")) {
		try {
			fso.DeleteFile(system32 + "\\SMSCFG.ini", true);
		} catch (e) {
			Log(logf, " !! No se pudo eliminar SMSCFG.ini");
		}
	}
}

function InstallClient () {
    Log(logf, "* Instalando cliente");

    try {
        var wmi = GetObject("winmgmts:\\\\.\\root\\cimv2");
    } catch (e) {
        Log(logf, "  !! No se pudo acceder a WMI");
        Exit(-1, "No se pudo acceder a WMI luego de la reparación");
    }

    try {
        Log(logf, "  + Mapeando share del cliente");
        net.MapNetworkDrive("", "\\\\trabajo\\netlogon\\clientesms", false);
    } catch (e) {
        Log(logf, " !! No se pudo mapear el share del cliente.");
        Exit(-5, "Error al mapear el share");
    }

    try {
        fso.DeleteFolder(ccmsetupdir, true);
    } catch (e) {
        Log(logf, "  !! No se pudo eliminar la carpeta ccmsetup");
    }

    Log(logf, "  + Ejecutando instalador");
    shell.Run("\\\\trabajo\\netlogon\\clientesms\\ccmsetup.exe SMSSITECODE=AUTO SMSCACHESIZE=16240 /UsePKICert", 0, true);
    WScript.Sleep(900000);
    // Chequear si se instaló el cliente
    var col = wmi.ExecQuery("select * from Win32_Product where Name = \"Configuration Manager Client\"",
                             "WQL", wbemFlagForwardOnly | wbemFlagReturnWhenComplete);
    var en = new Enumerator(col);
    if (en.atEnd()) {
        Log(logf, "  !! No se pudo completar la instalación del cliente.");
        Exit(-3, "No se pudo instalar el cliente.");
    } else {
        Log(logf, "  + Cliente instalado correctamente");
    }
}

///////// COMIENZO DE EJECUCIÓN DEL SCRIPT

var logf = fso.CreateTextFile(windir + "\\Temp\\FixCliente.log", true, true);

Log(logf, "* Sincronizando hora con el dominio");
shell.Run("net time /set /yes", 0, true);
Log(logf, "* Matando ccmexec.exe, ccmsetup.exe y ccmrepair.exe");
shell.Run("net stop ccmsetup /y", 0, true);
shell.Run("net stop ccmexec /y", 0, true);
shell.Run("net stop SCCMHealthWMI /y", 0, true);
shell.Run("pskill.exe -t ccmexec.exe", 0, true);
shell.Run("pskill.exe -t ccmsetup.exe", 0, true);
shell.Run("pskill.exe -t ccmrepair.exe", 0, true);
shell.Run("pskill.exe -t gpupdate.exe", 0, true);
shell.Run("pskill.exe -t SCCMHealth", 0, true);

ConfigureRemoteRegistry();
EnableRemoting();
FixPolicy();
FixMSI();
FixWMI();
UninstallClient();
DeleteCCMNamespace();
//RemoveCertificates();
InstallClient();

Exit(0, "Reparación correcta");