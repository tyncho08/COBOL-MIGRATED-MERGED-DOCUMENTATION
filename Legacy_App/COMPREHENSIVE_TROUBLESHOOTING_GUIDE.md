# Guía Completa de Resolución de Problemas - Sistema ACAS

## Tabla de Contenidos
- [Problemas Más Comunes](#problemas-más-comunes)
- [Diagnóstico por Síntomas](#diagnóstico-por-síntomas)
- [Soluciones Paso a Paso](#soluciones-paso-a-paso)
- [Issues Conocidos Sin Solución](#issues-conocidos-sin-solución)
- [Procedimientos de Recuperación](#procedimientos-de-recuperación)
- [Códigos de Error del Sistema](#códigos-de-error-del-sistema)
- [Herramientas de Diagnóstico](#herramientas-de-diagnóstico)

---

## Problemas Más Comunes

### 1. Problema de Creación Automática de system.dat
**Problema**: ACAS no crea automáticamente el archivo system.dat en el primer inicio.

**Síntomas**:
- Error "SY102 Read Err 1 = 23" al ejecutar ACAS
- Error de archivo no encontrado (file status 35)
- El sistema termina antes de mostrar la pantalla de configuración

**Diagnóstico**:
- El archivo ~/ACAS/system.dat no existe
- El proceso ba000-mapser no se ejecuta correctamente
- Error en la lógica de acas000-Open-Input

**Solución Inmediata**:
```bash
# Opción 1: Crear archivo vacío
touch ~/ACAS/system.dat

# Opción 2: Usar el programa de creación
cd ~/ACAS
rm -f system.dat
cobc -x /ruta/al/create-system-dat.cbl -o create-system-dat
./create-system-dat
```

### 2. Variables de Entorno No Configuradas
**Problema**: Error "SY009 Environment variables not yet set up"

**Síntomas**:
- Sistema no inicia
- Mensaje de variables de entorno no configuradas
- No encuentra módulos del sistema

**Solución**:
```bash
# Usar el script wrapper (recomendado)
~/bin/acas-run.sh

# O configurar manualmente
export ACAS_LEDGERS="$HOME/ACAS"
export ACAS_BIN="$HOME/bin"
export COB_LIBRARY_PATH="$HOME/bin:./common"
```

### 3. Módulos No Encontrados
**Problema**: Error "module 'acas000' not found"

**Síntomas**:
- Error al cargar bibliotecas COBOL
- Módulos no encontrados durante ejecución
- Sistema termina con error de carga

**Solución**:
```bash
# Verificar y configurar COB_LIBRARY_PATH
export COB_LIBRARY_PATH="$HOME/bin:./common:$HOME/Desktop/Demos/ACAS-Nightly/common"

# Verificar que los archivos .so/.dylib existen
ls -la ~/bin/*.dylib
ls -la ~/Desktop/Demos/ACAS-Nightly/common/*.dylib
```

### 4. Terminal No Responde
**Problema**: El programa "se cuelga" esperando entrada del usuario

**Síntomas**:
- Pantalla en blanco o congelada
- No responde a teclas
- Cursor no se mueve

**Solución**:
```bash
# Salir del programa
Ctrl+C

# Verificar requisitos del terminal
# Terminal debe ser al menos 80x24 caracteres
stty size  # debe mostrar al menos "24 80"

# Usar terminal interactivo (Terminal.app o iTerm2)
# NO usar desde scripts o procesos batch
```

---

## Diagnóstico por Síntomas

### Sistema No Inicia

| Síntoma | Causa Probable | Solución |
|---------|----------------|----------|
| "SY009 Environment variables not set" | Variables de entorno faltantes | Usar acas-run.sh o configurar manualmente |
| "module not found" | COB_LIBRARY_PATH incorrecto | Verificar y ajustar rutas de bibliotecas |
| "SY102 Read Err 1 = 23" | system.dat no existe o corrupto | Recrear system.dat |
| Terminal requirements not met | Tamaño terminal < 80x24 | Redimensionar terminal |

### Errores Durante Ejecución

| Error | Descripción | Acción |
|-------|-------------|---------|
| FS-Reply = 35 | Archivo no encontrado | Verificar estructura de directorios |
| FS-Reply = 22 | Llave duplicada | Revisar integridad de archivos |
| FS-Reply = 21 | Acceso denegado | Verificar permisos de archivos |
| SM004 | Error SQL en procedimientos | Verificar configuración RDB |

### Problemas de Pantalla/Display

| Síntoma | Causa | Solución |
|---------|--------|----------|
| Caracteres extraños | Codificación incorrecta | Configurar terminal UTF-8 |
| Campos desalineados | Terminal muy pequeño | Redimensionar a 80x24 mínimo |
| No acepta input numérico | Problema GnuCOBOL | Usar rutinas C experimentales |

---

## Soluciones Paso a Paso

### Solución 1: Configuración Inicial Completa

```bash
# Paso 1: Verificar instalación
ls -la ~/bin/ACAS ~/bin/acas-run.sh

# Paso 2: Crear directorios necesarios
mkdir -p ~/ACAS/archives ~/ACAS/temp-backups

# Paso 3: Configurar permisos
chmod +x ~/bin/acas-run.sh
chmod +x ~/bin/ACAS ~/bin/irs ~/bin/sales ~/bin/purchase ~/bin/stock

# Paso 4: Verificar bibliotecas
ls -la ~/bin/*.dylib
export COB_LIBRARY_PATH="$HOME/bin:./common"

# Paso 5: Ejecutar primera vez
~/bin/acas-run.sh
```

### Solución 2: Recuperación de system.dat Corrupto

```bash
# Paso 1: Hacer backup si existe
if [ -f ~/ACAS/system.dat ]; then
    cp ~/ACAS/system.dat ~/ACAS/system.dat.backup.$(date +%Y%m%d_%H%M%S)
fi

# Paso 2: Remover archivo corrupto
rm -f ~/ACAS/system.dat

# Paso 3: Recrear archivo base
cd ~/ACAS
touch system.dat

# Paso 4: Ejecutar ACAS para inicialización
~/bin/acas-run.sh

# El sistema debe mostrar pantalla de configuración inicial
# Completar configuración de empresa
```

### Solución 3: Recompilación Completa del Sistema

```bash
# Solo si hay problemas persistentes con módulos

# Paso 1: Navegar al código fuente
cd /ruta/al/codigo/fuente/ACAS-Nightly

# Paso 2: Limpiar compilación anterior
rm -f ~/bin/ACAS ~/bin/irs ~/bin/sales ~/bin/purchase ~/bin/stock ~/bin/general

# Paso 3: Recompilar (sin RDBMS)
./comp-all-no-rdbms.sh

# Paso 4: Reinstalar
./install-ACAS-preinstalled.sh

# Paso 5: Verificar instalación
~/bin/acas-run.sh
```

---

## Issues Conocidos Sin Solución

### 1. Limitaciones de GnuCOBOL 3.2
- **Problema**: Input numérico con decimales puede fallar
- **Impacto**: Entrada de datos monetarios problemática
- **Workaround**: Usar rutinas C experimentales cuando estén disponibles
- **Estado**: En desarrollo activo

### 2. Archivo system.dat - Lógica de Creación
- **Problema**: ba000-mapser no siempre se ejecuta en primer inicio
- **Causa Raíz**: Error en manejo de disk-error-display en sys002.cbl
- **Impacto**: Requiere intervención manual en algunos casos
- **Estado**: Necesita refactoring de acas000.cbl

### 3. Problemas de Concurrencia Multi-Usuario
- **Problema**: Llaves duplicadas en ambiente multi-usuario
- **Impacto**: Corrupción de datos en uso simultáneo
- **Workaround**: Uso de un solo usuario por vez
- **Estado**: Mejoras implementadas en v3.02, testing continuo

### 4. Dependency en Terminal Específico
- **Problema**: Requiere terminal interactivo específico (80x24 mínimo)
- **Impacto**: No puede ejecutarse en batch o scripts automatizados
- **Limitación**: Arquitectura fundamental del sistema
- **Estado**: Por diseño, no será cambiado

---

## Procedimientos de Recuperación

### Recovery Nivel 1: Problemas Menores

```bash
# Para errores de archivos temporales o locks
cd ~/ACAS

# Limpiar archivos temporales
rm -f *.tmp *.lock *.temp

# Verificar permisos
chmod 644 *.dat
chmod 755 ~/bin/ACAS*

# Reiniciar sistema
~/bin/acas-run.sh
```

### Recovery Nivel 2: Corrupción de Datos

```bash
# Cuando hay errores de lectura/escritura de archivos

# 1. Parar ACAS inmediatamente
# Ctrl+C si está ejecutando

# 2. Backup de estado actual
cd ~/ACAS
tar -czf recovery_backup_$(date +%Y%m%d_%H%M%S).tar.gz *.dat

# 3. Verificar integridad de archivos principales
ls -la *.dat
# Buscar archivos de tamaño 0 o fechas anómalas

# 4. Restaurar desde backup más reciente
cd archives/
ls -la *.tar.gz | tail -5
# Restaurar el backup más reciente válido

# 5. Verificar system.dat
file system.dat
# Debe ser "data" no "empty" o "ASCII text"
```

### Recovery Nivel 3: Falla Catastrófica

```bash
# Cuando el sistema está completamente corrupto

# 1. Backup completo del estado corrupto
tar -czf DISASTER_BACKUP_$(date +%Y%m%d_%H%M%S).tar.gz ~/ACAS/

# 2. Reinstalación limpia
rm -rf ~/ACAS/*
mkdir -p ~/ACAS/archives ~/ACAS/temp-backups

# 3. Recompilar sistema completo
cd /ruta/codigo/fuente
./comp-all-no-rdbms.sh
./install-ACAS-preinstalled.sh

# 4. Recuperar datos desde el último backup válido conocido
# (Requiere identificar backup válido manualmente)

# 5. Reconstruir configuración
~/bin/acas-run.sh
# Completar configuración inicial desde cero
```

---

## Códigos de Error del Sistema

### Errores de Sistema (SY001-SY999)

| Código | Descripción | Solución |
|--------|-------------|----------|
| SY001 | System file read error | Verificar permisos system.dat |
| SY004 | Problem opening system file | Recrear system.dat |
| SY006 | Program arguments limited | Revisar parámetros de línea de comandos |
| SY007 | Program arguments incorrect | Verificar sintaxis de argumentos |
| SY008 | Note message & Hit return | Informativo, presionar Enter |
| SY009 | Environment variables not set | Configurar variables de entorno |
| SY019 | DB not configured in param | Configurar parámetros RDBMS o usar solo archivos |
| SY102 | Read Err 1 = 23 | Archivo system.dat faltante o corrupto |

### Errores de Archivos (FS-Reply Codes)

| Código | Significado | Causa Común | Acción |
|--------|-------------|-------------|---------|
| 00 | Successful operation | N/A | Continuar |
| 10 | End of file | Lectura completa | Normal |
| 21 | Access denied | Permisos | chmod 644 archivo |
| 22 | Duplicate key | Llave ya existe | Verificar unicidad |
| 23 | Record not found | No existe | Crear registro |
| 35 | File not found | Archivo faltante | Crear archivo |
| 99 | Generic error | Varios | Investigar causa |

### Errores de Base de Datos (SM001-SM999)

| Código | Descripción | Causa | Solución |
|--------|-------------|-------|----------|
| SM004 | SQL Error in procedures | Error SQL | Verificar configuración DB |
| SM901 | Note error and hit return | Error informativo | Revisar detalles |

### Errores por Subsistema

#### General Ledger (GL001-GL999)
- GL010: Duplicate keys found
- GL011: Error on processing  
- GL012: Error opening file
- GL013: Error reading file
- GL014: Error writing table

#### Sales Ledger (SL001-SL999)
- SL015: Error en procesamiento de facturas
- SL020: Problema con archivo de clientes

#### Stock Control (ST001-ST999)
- ST010: Error en control de inventario
- ST020: Problema con archivo de productos

---

## Herramientas de Diagnóstico

### Script de Diagnóstico Automático

```bash
#!/bin/bash
# ACAS System Diagnostic Script

echo "=== ACAS System Diagnostic ==="
echo "Date: $(date)"
echo

# Check environment
echo "1. Environment Variables:"
echo "ACAS_LEDGERS: ${ACAS_LEDGERS:-NOT SET}"
echo "ACAS_BIN: ${ACAS_BIN:-NOT SET}"
echo "COB_LIBRARY_PATH: ${COB_LIBRARY_PATH:-NOT SET}"
echo

# Check files
echo "2. Critical Files:"
echo -n "~/bin/ACAS: "
[ -f ~/bin/ACAS ] && echo "EXISTS" || echo "MISSING"
echo -n "~/bin/acas-run.sh: "
[ -f ~/bin/acas-run.sh ] && echo "EXISTS" || echo "MISSING"
echo -n "~/ACAS/system.dat: "
[ -f ~/ACAS/system.dat ] && echo "EXISTS ($(stat -f%z ~/ACAS/system.dat 2>/dev/null || echo "0") bytes)" || echo "MISSING"
echo

# Check directories
echo "3. Directory Structure:"
echo -n "~/ACAS: "
[ -d ~/ACAS ] && echo "EXISTS" || echo "MISSING"
echo -n "~/ACAS/archives: "
[ -d ~/ACAS/archives ] && echo "EXISTS" || echo "MISSING"
echo -n "~/bin: "
[ -d ~/bin ] && echo "EXISTS" || echo "MISSING"
echo

# Check permissions
echo "4. Permissions:"
ls -la ~/bin/ACAS* 2>/dev/null | head -5
echo

# Check libraries
echo "5. COBOL Libraries:"
ls -la ~/bin/*.dylib 2>/dev/null | wc -l | sed 's/^/Libraries found: /'
echo

# Terminal check
echo "6. Terminal Requirements:"
echo "Current size: $(stty size 2>/dev/null || echo "unknown")"
echo "Required: 24 80 (minimum)"
echo

echo "=== Diagnostic Complete ==="
```

### Comandos de Verificación Rápida

```bash
# Verificación rápida del sistema
alias acas-check='ls -la ~/bin/ACAS* && ls -la ~/ACAS/*.dat && echo "ACAS_LEDGERS=$ACAS_LEDGERS" && echo "Terminal: $(stty size)"'

# Log viewer para errores
alias acas-logs='find ~/ACAS -name "*.log" -exec tail -20 {} + 2>/dev/null'

# Status completo
alias acas-status='ps aux | grep -i acas; ls -la ~/ACAS/; df -h ~/ACAS'
```

### Herramientas de Monitoreo

```bash
# Monitor de archivos durante ejecución
fswatch ~/ACAS/ | while read event; do
    echo "$(date): File changed: $event"
done

# Monitor de procesos ACAS
watch 'ps aux | grep -E "(ACAS|irs|sales|purchase|stock)" | grep -v grep'
```

---

## Contacto y Soporte

### Información de Issues Conocidos
- Muchos problemas están documentados en los changelogs individuales
- Buscar por programa específico en archivos .cbl para historial de bugs
- Version actual: v3.02 (desarrollo activo hasta 2025)

### Desarrollo Activo
- Sistema en desarrollo y testing continuo
- Nuevas features: Back Orders, Autogen invoices
- Migración continua de RM COBOL a GnuCOBOL

### Limitaciones Documentadas
1. **No es sistema batch**: Requiere interacción humana
2. **Single-user recomendado**: Problemas de concurrencia conocidos
3. **Terminal específico**: 80x24 mínimo, interactivo
4. **Numeric input**: Limitaciones de GnuCOBOL en decimal input

---

## Notas Finales

Este sistema tiene más de 45 años de desarrollo continuo (1976-2025) y representa una migración activa de COBOL comercial a open source. Muchos "problemas" son en realidad características por diseño del sistema original.

Para problemas no cubiertos en esta guía:
1. Verificar changelogs por módulo específico
2. Revisar TODO files para limitaciones conocidas
3. Consultar código fuente para comportamientos específicos

**Estado del sistema**: FUNCIONAL y en USO ACTIVO según documentación más reciente.