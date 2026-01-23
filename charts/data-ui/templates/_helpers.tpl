{{/* Generate standard labels */}}
{{- define "data-uid.labels" -}}
app: data-uid-{{ .Values.environment }}
environment: {{ .Values.environment }}
{{- end -}}

{{/* Same for postgres */}}
{{- define "postgres.labels" -}}
app: postgres-{{ .Values.environment }}
{{- end -}}
